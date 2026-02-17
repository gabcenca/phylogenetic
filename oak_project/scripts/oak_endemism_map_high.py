#!/usr/bin/env python3
"""
Script to map cells with highest weighted endemism (>0.8) in Mexico
with biogeographic provinces overlay
"""

import numpy as np
import matplotlib.pyplot as plt
import rasterio
from rasterio.plot import show
import geopandas as gpd
from matplotlib.patches import Rectangle
from matplotlib import patheffects

# ==============================================================================
# CONFIGURATION - Update these paths to your files
# ==============================================================================
RASTER_PATH = r"D:\oak_project\data\wendemism_tif_0.315.tif"  # UPDATE TO YOUR ENDEMISM RASTER
SHAPEFILE_PATH = r"D:\oak_project\data\pbiogmx17gw.shp"
ENDEMISM_THRESHOLD = 0.8
OUTPUT_FILE = r"D:\oak_project\outputs\oak_endemism_map.png"

# ==============================================================================
# LOAD DATA
# ==============================================================================
print("Loading raster data...")
with rasterio.open(RASTER_PATH) as src:
    endemism = src.read(1)  # Read first band
    transform = src.transform
    crs = src.crs
    nodata = src.nodata
    
    # Handle nodata values
    if nodata is not None:
        endemism_masked = np.ma.masked_equal(endemism, nodata)
    else:
        endemism_masked = np.ma.masked_invalid(endemism)

print("Loading biogeographic provinces...")
provinces = gpd.read_file(SHAPEFILE_PATH)

# Ensure provinces are in same CRS as raster
if provinces.crs != crs:
    print(f"Reprojecting provinces from {provinces.crs} to {crs}")
    provinces = provinces.to_crs(crs)

# ==============================================================================
# IDENTIFY HIGH ENDEMISM CELLS
# ==============================================================================
print(f"Finding cells with weighted endemism > {ENDEMISM_THRESHOLD}...")
high_endemism_mask = endemism_masked > ENDEMISM_THRESHOLD

# Get row, col indices of high endemism cells
rows, cols = np.where(high_endemism_mask)
print(f"Found {len(rows)} cells with weighted endemism > {ENDEMISM_THRESHOLD}")

# Get the actual endemism values for these cells
endemism_values = endemism[rows, cols]

# Convert pixel coordinates to geographic coordinates (cell centers)
coords = []
for row, col in zip(rows, cols):
    # Get the center of the cell
    x, y = rasterio.transform.xy(transform, row, col, offset='center')
    coords.append((x, y))

# ==============================================================================
# CREATE THE MAP
# ==============================================================================
print("Creating map...")
fig, ax = plt.subplots(figsize=(14, 10))

# Highlight only the high endemism cells (background removed)
high_endemism_raster = np.ma.masked_where(~high_endemism_mask, endemism_masked)
im = show(high_endemism_raster, transform=transform, ax=ax, 
          cmap='YlOrRd', alpha=0.8, vmin=ENDEMISM_THRESHOLD, 
          vmax=np.nanmax(endemism_masked))

# Plot biogeographic provinces
provinces.plot(ax=ax, facecolor='none', edgecolor='black', linewidth=1.5, 
               linestyle='--', alpha=0.7)

# Add province labels
for idx, row in provinces.iterrows():
    centroid = row.geometry.centroid
    ax.annotate(text=row['JJM2017'], xy=(centroid.x, centroid.y),
                ha='center', fontsize=8, style='italic', alpha=0.7,
                bbox=dict(boxstyle='round,pad=0.3', facecolor='white', 
                         edgecolor='gray', alpha=0.7))

# Add cell value labels for high endemism cells
print("Adding value labels...")
for (x, y), value in zip(coords, endemism_values):
    # Create text with white outline for better visibility
    text = ax.text(x, y, f'{value:.2f}', 
                   ha='center', va='center',
                   fontsize=8, fontweight='bold', color='darkred')
    text.set_path_effects([patheffects.withStroke(linewidth=2, foreground='white')])

# Add colorbar
cbar = plt.colorbar(im.images[0], ax=ax, shrink=0.6, pad=0.02)
cbar.set_label('Weighted Endemism Index', rotation=270, labelpad=20, fontsize=12)

# Formatting
ax.set_xlabel('Longitude', fontsize=11)
ax.set_ylabel('Latitude', fontsize=11)
ax.set_title(f'Oak Weighted Endemism Hotspots in Mexico (Index > {ENDEMISM_THRESHOLD})\n' + 
             'with Biogeographic Provinces', fontsize=14, fontweight='bold', pad=15)

# Add grid
ax.grid(True, alpha=0.3, linestyle=':', linewidth=0.5)

# Add legend
from matplotlib.patches import Patch
legend_elements = [
    Patch(facecolor='none', edgecolor='black', linewidth=1.5, 
          linestyle='--', label='Biogeographic Provinces')
]
ax.legend(handles=legend_elements, loc='lower left', fontsize=10)

plt.tight_layout()

# ==============================================================================
# SAVE AND DISPLAY
# ==============================================================================
print(f"Saving map to {OUTPUT_FILE}...")
plt.savefig(OUTPUT_FILE, dpi=300, bbox_inches='tight')
print("Done!")

# Display statistics
print(f"\n{'='*60}")
print("SUMMARY STATISTICS")
print(f"{'='*60}")
print(f"Total cells with weighted endemism > {ENDEMISM_THRESHOLD}: {len(rows)}")
print(f"Maximum endemism value: {np.nanmax(endemism_values):.3f}")
print(f"Minimum endemism value (in filtered cells): {np.nanmin(endemism_values):.3f}")
print(f"Mean endemism (in filtered cells): {np.nanmean(endemism_values):.3f}")
print(f"{'='*60}\n")

plt.show()
