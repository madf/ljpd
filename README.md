# ljpd - LiveJournal Photo dump

This utility is inspired by https://github.com/ghewgill/ljdump

`ljdump` is used to backup your blog records from LiveJournal and similar sites. Unfortunately, it does not support backing up images, which are stored separately from the blog.

## Building

```
stack setup
stack build
```

## Running

```
stack run <username>
```
or
```
stack run <username> <dest-dir>
```

Unless manually specified, all albums and images are stored in the `dest` directory.

## Backup structure

```
A-NNN
 |
 +- descr.json
 +- P-XXX
 |   |
 |   +- descr.json
 |   +- original.jpeg
 |   +- thumbnail.jpeg
 +- P-YYY
  ...
 +- P-ZZZ
A-MMM
 ...
A-VVV
```

`A-NNN` - album folder. `NNN` - album id. `descr.json` inside the folder contains essential album information:
 * `id` - album id. Matches `NNN`.
 * `count` - number of images in the album.
 * `name` - album name (may be empty).
 * `description` - album description (may be empty).

`P-XXX` - image folder. XXX - image id. `descr.json` inside the folder contains image information:
 * `id` - image id. Matches `XXX`.
 * `width` - image width.
 * `height` - image height.
 * `url` - original image URL.
 * `thumbnail` - original thumbnail URL.
 * `name` - image name (if any).
 * `description` - image description (if any).

 Image folder also contains two images: `original.jpeg` and `thumbnail.jpeg`. Image format and extension may differ. The `original` file contains the image obtained from the `url` parameter. The `thumbnail` is obtained from the `thumbnail` parameter.

 For now this utility is only capable of dumping images with public visibility. Dumping private images may be added later and will require providing user password.
