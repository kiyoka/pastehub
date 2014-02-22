Design Memo
=======================

## Directory tree ( Dropbox )

If There are three hosts <hostname1> ... <hostname3>, files are created like this.

> ~/Dropbox/pastehub/<hostname1>.dat
> ~/Dropbox/pastehub/<hostname2>.dat
> ~/Dropbox/pastehub/<hostname3>.dat

## file format

### first line
JSON format

example:
> { hostname: hostname1, createDate: "2014 ....", bodySize : 14, encodedBodySize : 20 }

- hostname
  hostname of this paste.

- createDate
  create date of this paste.

- bodySize
  data size of decoded string.

- encodedBodySize
  data size of raw second line.


### second line
data body ( base64 with no-line-break)
same as result of eval (base64-encode-string "STRING" t) in Emacs-24.

example:
This is encode data of "abcdefghijklmn"
> YWJjZGVmZ2hpamtsbW4=

