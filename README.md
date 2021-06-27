# Photosort

Very simple [Chicken Scheme](https://call-cc.org) program that copies jpeg images from one directory into a new directory with a date structure.

Only JPEGs will be moved, and only those with Exif data. This was written to bring some sense of order to my Dropbox "Camera Uploads" folder.

## Example

`$ photosort /tmp/directory-a /tmp/directory-b`

**Directory A**:

```
file1.jpg
file2.jpg
file3.jpg
```

**Directory B**:

Files will be checked for date information and moved into a structure like:

```
<YEAR>
 └── <MONTH>
     └── <FILENAME>
     └── ...
```

`YEAR` and `MONTH` are extracted from Exif tags on the photo. If none are present then we try to grab them from the filename, hoping that your filenames are something like `2021-05-01 10:45:21.jpg`.

### SQLite

This program also stores information about the moved photos in a SQLite DB within your `~/.cache` directory. This might be useful if you want a record of what moved where, etc.
