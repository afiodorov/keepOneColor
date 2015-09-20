## KeepOneColor

A small utility for removing all but one color. Could be used in subtitle
extraction.

Example:

```shell
cabal run img.png out.png
```

![before](https://raw.githubusercontent.com/afiodorov/keepOneColor/master/img.png)
![after](https://raw.githubusercontent.com/afiodorov/keepOneColor/master/out.png)

KeepOneColor

------

Usage: keepOneColor INPUT OUTPUT [-k|--keep (R, G, B)]
                    [-b|--background (R, G, B)] [-r|--replace (R, G, B)]
  Removes all but one color

Available options:
  -h,--help                Show this help text
  -k,--keep (R, G, B)      rgb of a color to keep (default: (255, 255, 0))
  -b,--background (R, G, B)
                           rgb of a background color (default: (255, 255, 255))
  -r,--replace (R, G, B)   rgb of a replace color (default: (0, 0, 0))
