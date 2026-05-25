This script makes it easy to scan a given directory for images or videos.

Found images are displayed in a browser like Chrome or Firefox in a NxM grid, making it
possible to browse through a large number of images quickly.

User can go to the next or previous pages by using the arrow keys.

The script detects newly created images using a filesystem observer,
and and adds them to the pipeline for vewing.

To install:

`$ pip install imageoverview`

`$ imageoverview -d ~/Pictures`

See images in a 10x15 grid for wider monitors:

`$ imageoverview -d ~/Pictures -D 10x15 grid`