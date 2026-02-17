#!/usr/bin/env python3
"""
Script to map cells with high phylogenetic diversity (≥280) in Mexico
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
RASTER_PATH = r"D:\oak_project\data\PD_tif_0.315.tif"  
SHAPEFILE_PATH = r"D:\oak_project\data\pbiogmx17gw.shp"
PD_THRESHOLD = 280  # Phylogenetic diversity threshold
OUTPUT_FILE = r"D:\oak_project\outputs\oak_high_phylodiversity_map.png"

# ==============================================================================
# LOAD DATA
# ==============================================================================
print("Loading raster data...")
with rasterio.open(RASTER_PATH) as src:
    phylodiversity = src.read(1)  # Read first band
    transform = src.transform
    crs = src.crs
    nodata = src.nodata
    
    # Handle nodata values
    if nodata is not None:
        phylodiversity_masked = np.ma.masked_equal(phylodiversity, nodata)
    else:
        phylodiversity_masked = np.ma.masked_invalid(phylodiversity)

print("Loading biogeographic provinces...")
provinces = gpd.read_file(SHAPEFILE_PATH)

# Ensure provinces are in same CRS as raster
if provinces.crs != crs:
    print(f"Reprojecting provinces from {provinces.crs} to {crs}")
    provinces = provinces.to_crs(crs)

# ==============================================================================
# IDENTIFY HIGH PHYLOGENETIC DIVERSITY CELLS
# ==============================================================================
print(f"Finding cells with phylogenetic diversity ≥ {PD_THRESHOLD}...")
high_pd_mask = phylodiversity_masked >= PD_THRESHOLD

# Get row, col indices of high PD cells
rows, cols = np.where(high_pd_mask)
print(f"Found {len(rows)} cells with phylogenetic diversity ≥ {PD_THRESHOLD}")

# Get the actual phylodiversity values for these cells
pd_values = phylodiversity[rows, cols]

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

# Highlight only the high phylogenetic diversity cells
high_pd_raster = np.ma.masked_where(~high_pd_mask, phylodiversity_masked)
im = show(high_pd_raster, transform=transform, ax=ax, 
          cmap='YlGnBu', alpha=0.8, vmin=PD_THRESHOLD, 
          vmax=np.nanmax(phylodiversity_masked))

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

# Add cell value labels for high PD cells
print("Adding value labels...")
for (x, y), value in zip(coords, pd_values):
    # Create text with white outline for better visibility
    text = ax.text(x, y, f'{value:.0f}', 
                   ha='center', va='center',
                   fontsize=8, fontweight='bold', color='darkblue')
    text.set_path_effects([patheffects.withStroke(linewidth=2, foreground='white')])

# Add colorbar
cbar = plt.colorbar(im.images[0], ax=ax, shrink=0.6, pad=0.02)
cbar.set_label('Phylogenetic Diversity Index', rotation=270, labelpad=20, fontsize=12)

# Formatting
ax.set_xlabel('Longitude', fontsize=11)
ax.set_ylabel('Latitude', fontsize=11)
ax.set_title(f'Oak High Phylogenetic Diversity Areas in Mexico (Index ≥ {PD_THRESHOLD})\n' + 
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
print(f"Total cells with phylogenetic diversity ≥ {PD_THRESHOLD}: {len(rows)}")
print(f"Maximum PD value: {np.nanmax(pd_values):.1f}")
print(f"Minimum PD value (in filtered cells): {np.nanmin(pd_values):.1f}")
print(f"Mean PD (in filtered cells): {np.nanmean(pd_values):.1f}")
print(f"{'='*60}\n")

plt.show()
