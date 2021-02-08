# This is a simple program to convert YouTube 360 videos
# into Equirectangular 360 Videos

# Original Author: Matthew Reed

import numpy as np
import cv2

"""
 - Map coordinates of "dst" onto "src"

 - Takes 2D array of (x,y) coordinates along with the
   shape of the source image.

 - Returns 2D array of (x,y) coordinates that fit within
   the source image dimensions
"""
def map_points(coords, shape):
    # Don't want to edit external things accidentally
    new = coords.copy().astype(float)

    # This may be backwards to what one would think,
    # but I want all pixels on the output to map to
    # at least one pixel on the source. To do this,
    # I work backwards with the coordinates (as if
    # I was converting a 360 image into YouTube's format)

    # Find 2D polar coordinates of equirectangular projection
    new = equirect_to_polar2d(new)

    # Find 3D polar coordinates that shoot through the
    # points and intersect with a unit cube
    new = polar2d_to_polar3d(new)

    # Polar to cartesian coordinates
    new = polar3d_to_cart3d(new)

    # Map each side of the unit cube to a cubemap
    new = cart3d_to_cubemap(new)

    # Adjust for the arctan() that YouTube does
    new = reverse_youtube_cubemap(new)

    # Map everything to the dimensions of the
    # source image
    new = fit_to_src(new, shape)

    return new


"""
 - Map equirectangular coordinates

 - Takes 2D array of (x,y) coordinates of any bound.

 - Returns 2D array of (longitude, latitude) coordinates
   between [0, 2pi] and [0, pi] respectively.
"""
def equirect_to_polar2d(coords):
    new = coords.copy()

    # Easy conversion from equirectangular coordinates.
    # Get latitude and longitude.
    new[:,0] = np.interp(new[:,0], (coords[:,0].min(), new[:,0].max()), (0, 2*np.pi))
    new[:,1] = np.interp(new[:,1], (coords[:,1].min(), new[:,1].max()), (0, np.pi))

    return new


"""
 - Map 2D (longitude, latitude) to 3D polar coordinates

 - Takes 2D array of (longitude, latitude) coordinates
   between [0, 2pi] and [0, pi] respectively

 - Returns 2D array of 3D polar (r, theta, phi) coordinates
   with bounds >2, [0, 2pi], and [0, pi] respectively. Each
   point should exist on the cube bounding cartesian
   [-1, -1, -1] to [1, 1, 1].
"""
def polar2d_to_polar3d(coords):
    new = np.zeros((coords.shape[0], 3), dtype=float)
    new[:,1:] = coords

    # Find the shortest length from the origin for a ray
    # through the given coordinates to intersect with a
    # plane on the unit cube.
    with np.errstate(divide='ignore'): # [1]
        x_dist = np.abs(1 / (np.sin(coords[:,1]) * np.cos(coords[:,0])))
        y_dist = np.abs(1 / (np.sin(coords[:,1]) * np.sin(coords[:,0])))
        z_dist = np.abs(1 / np.cos(coords[:,1]))

        new[:, 0] = np.amin([x_dist, y_dist, z_dist], axis=0)

    return new


"""
 - Convert polar coordinates to cartesian coordinates

 - Takes 2D array of 3D polar (r, theta, phi) coordinates
   with bounds none, [0, 2pi], and [0, pi] respectively

 - Returns 2D array of 3D cartesian (x, y, z) coordinates
"""
def polar3d_to_cart3d(coords):
    new = coords.copy()

    # For some reason, the cube's normals were flipped, so
    # I multiplied each coordinate by -1.
    new[:,0] = -coords[:,0] * np.sin(coords[:,2]) * np.cos(coords[:,1])
    new[:,1] = -coords[:,0] * np.sin(coords[:,2]) * np.sin(coords[:,1])
    new[:,2] = -coords[:,0] * np.cos(coords[:,2])

    return new


"""
 - Map faces of cube cartesian

 - Takes 2D array of 3D cartesian (x, y, z) coordinates
   that reside on the cube spanning [-1, -1, -1] to
   [1, 1, 1]

 - Returns 2D array of 2D cartesian (x, y) coordinates
   with bounds [0, 3] and [0, 2] respectively
"""
def cart3d_to_cubemap(coords):
    new = np.zeros((coords.shape[0], 5), dtype=float)
    new[:,2:] = coords

    # This accounts for any floating-point errors
    tolerance = 0.000001

    # Map from [-1,1] to [0,1]
    new = (new + 1) / 2

    # For each face on the cube, map it to the
    # correct part of the image with the correct
    # orientation.
    section = np.where(new[:,3] <= tolerance)
    new[section,0] = new[section,2]
    new[section,1] = new[section,4]

    section = np.where(new[:,2] >= 1 - tolerance)
    new[section,0] = new[section,3] + 1
    new[section,1] = new[section,4]

    section = np.where(new[:,3] >= 1 - tolerance)
    new[section,0] = 1 - new[section,2] + 2
    new[section,1] = new[section,4]

    section = np.where(new[:,4] >= 1 - tolerance)
    new[section,0] = 1 - new[section,2]
    new[section,1] = 1 - new[section,3] + 1

    section = np.where(new[:,2] <= tolerance)
    new[section,0] = 1 - new[section,4] + 1
    new[section,1] = 1 - new[section,3] + 1

    section = np.where(new[:,4] <= tolerance)
    new[section,0] = new[section,2] + 2
    new[section,1] = 1 - new[section,3] + 1

    return new[:,:2]


"""
 - Fix YouTube's adjustments to a cubemap

 - Takes 2D array of 2D cartesian (x, y) coordinates
   where each value between [i, i+1] point to the same
   side of the mapped cube.

 - Returns 2D array of 2D cartesian (x, y) coordinates
   with same bounds as input.
"""
def reverse_youtube_cubemap(coords):
    new = coords.copy()

    # Make each face into squares of [-1,1]
    new = new - np.floor(new)
    new = (new - 0.5) * 2
    new = np.interp(new, (new.min(), new.max()), (-1, 1))

    # Counteract YouTube's arctan()
    new = np.arctan(new)

    # Re-map the faces to the correct place
    # on the image
    new = np.interp(new, (new.min(), new.max()), (0, 1))
    new = np.floor(coords) + new

    return new


"""
 - Map coordinates between the bounds of the source image

 - Takes 2D array of 2D cartesian (x, y) coordinates
   that map to a generic YouTube 3x2 layout

 - Returns 2D array of 2D cartesian (x, y) coordinates
   with the same bounds as the source image
"""
def fit_to_src(coords, shape):
    new = coords.copy()

    # Fit the coordinates onto the bounds
    # of the source image
    new[:,0] = np.interp(new[:,0], (new[:,0].min(), new[:,0].max()), (0, shape[1] - 1))
    new[:,1] = np.interp(new[:,1], (new[:,1].min(), new[:,1].max()), (0, shape[0] - 1))

    return new

"""
MAIN
"""
def main():
    src = cv2.imread('input.png')
    dst = np.zeros((720, 1280, 3), dtype=np.uint8)

    dst_shape = dst.shape
    src_shape = src.shape

    # Generate a list of (x, y) coords
    coords_dst = np.indices((dst_shape[1], dst_shape[0])).swapaxes(0,2).swapaxes(0,1).reshape((-1, 2)) # [2]

    # Map each (x, y) coordinate to (x, y) coordinates
    # on the source image.
    coords_src = map_points(coords_dst, src_shape).astype(int)

    # Translate (x, y) coords to indeces
    points_src = (coords_src[:,1]*src_shape[1] + coords_src[:,0]) * 3
    points_src = np.transpose([points_src] * 3) # [3]
    points_src[:,1] = points_src[:,1] + 1
    points_src[:,2] = points_src[:,2] + 2

    points_dst = (coords_dst[:,1]*dst_shape[1] + coords_dst[:,0]) * 3
    points_dst = np.transpose([points_dst] * 3) # [3]
    points_dst[:,1] = points_dst[:,1] + 1
    points_dst[:,2] = points_dst[:,2] + 2

    # Apply the indeces
    dst = dst.flatten()
    dst[points_dst] = src.flatten()[points_src]
    dst = dst.reshape(dst_shape)

    # Display the final result
    cv2.imshow('dst', dst)
    cv2.waitKey(0)

if __name__ == "__main__":
    main()

# Sources
# [1] https://stackoverflow.com/questions/29950557/ignore-divide-by-0-warning-in-numpy
# [2] https://stackoverflow.com/questions/18359671/fastest-method-to-create-2d-numpy-array-whose-elements-are-in-range
# [3] https://www.kite.com/python/answers/how-to-repeat-an-array-as-a-row-or-column-using-numpy-in-python