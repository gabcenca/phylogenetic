#!/usr/bin/env python3
"""
Script to map cells with highest oak richness (≥35) in Mexico
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
RASTER_PATH = r"D:\oak_project\data\richness_tif_0.315.tif"
SHAPEFILE_PATH = r"D:\oak_project\data\pbiogmx17gw.shp"
RICHNESS_THRESHOLD = 29
OUTPUT_FILE = r"D:\oak_project\outputs\oak_richness_map.png"

# ==============================================================================
# LOAD DATA
# ==============================================================================
print("Loading raster data...")
with rasterio.open(RASTER_PATH) as src:
    richness = src.read(1)  # Read first band
    transform = src.transform
    crs = src.crs
    nodata = src.nodata
    
    # Handle nodata values
    if nodata is not None:
        richness_masked = np.ma.masked_equal(richness, nodata)
    else:
        richness_masked = np.ma.masked_invalid(richness)

print("Loading biogeographic provinces...")
provinces = gpd.read_file(SHAPEFILE_PATH)

# Ensure provinces are in same CRS as raster
if provinces.crs != crs:
    print(f"Reprojecting provinces from {provinces.crs} to {crs}")
    provinces = provinces.to_crs(crs)

# ==============================================================================
# IDENTIFY HIGH RICHNESS CELLS
# ==============================================================================
print(f"Finding cells with richness >= {RICHNESS_THRESHOLD}...")
high_richness_mask = richness_masked >= RICHNESS_THRESHOLD

# Get row, col indices of high richness cells
rows, cols = np.where(high_richness_mask)
print(f"Found {len(rows)} cells with richness >= {RICHNESS_THRESHOLD}")

# Get the actual richness values for these cells
richness_values = richness[rows, cols]

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

# Highlight only the high richness cells (background removed)
high_richness_raster = np.ma.masked_where(~high_richness_mask, richness_masked)
im = show(high_richness_raster, transform=transform, ax=ax, 
          cmap='RdYlGn', alpha=0.8, vmin=RICHNESS_THRESHOLD, 
          vmax=np.nanmax(richness_masked))

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

# Add cell value labels for high richness cells
print("Adding value labels...")
for (x, y), value in zip(coords, richness_values):
    # Create text with white outline for better visibility
    text = ax.text(x, y, f'{int(value)}', 
                   ha='center', va='center',
                   fontsize=8, fontweight='bold', color='darkred')
    text.set_path_effects([patheffects.withStroke(linewidth=2, foreground='white')])

# Add colorbar
cbar = plt.colorbar(im.images[0], ax=ax, shrink=0.6, pad=0.02)
cbar.set_label('Oak Species Richness', rotation=270, labelpad=20, fontsize=12)

# Formatting
ax.set_xlabel('Longitude', fontsize=11)
ax.set_ylabel('Latitude', fontsize=11)
ax.set_title(f'Oak Species Richness in Mexico (Richness ≥ {RICHNESS_THRESHOLD})\n' + 
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
print(f"Total cells with richness >= {RICHNESS_THRESHOLD}: {len(rows)}")
print(f"Maximum richness value: {np.nanmax(richness_values):.0f}")
print(f"Minimum richness value (in filtered cells): {np.nanmin(richness_values):.0f}")
print(f"Mean richness (in filtered cells): {np.nanmean(richness_values):.1f}")
print(f"{'='*60}\n")

plt.show()
