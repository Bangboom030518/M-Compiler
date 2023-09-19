> TODO: string types go brrrr!

The library used to encode and decode strings, allowing the support of encoding methods other than `UTF-8`.

## Method

```
export enum Method {
    UTF_8,
    UTF_16,
    UTF_32,
    ASCII,
    WINDOWS_1250,
    WINDOWS_1251,
    WINDOWS_1252,
    WINDOWS_1253,
    WINDOWS_1254,
    WINDOWS_1255,
    WINDOWS_1256,
    WINDOWS_1257,
    WINDOWS_1258,
    MAC_OS_ROMAN,
    KOI8_R,
    KOI8_U,
    KOI7,
    MIK,
    ISCII,
    TSCII,
    VISCII,
    CP437,
    CP720,
    CP737,
    CP850,
    CP852,
    CP855,
    CP857,
    CP858,
    CP860,
    CP861,
    CP862,
    CP863,
    CP865,
    CP866,
    CP869,
    CP872,
    ISO_8859_1,
    ISO_8859_2,
    ISO_8859_3,
    ISO_8859_4,
    ISO_8859_5,
    ISO_8859_6,
    ISO_8859_7,
    ISO_8859_8,
    ISO_8859_9,
    ISO_8859_10,
    ISO_8859_11,
    ISO_8859_12,
    ISO_8859_13,
    ISO_8859_14,
    ISO_8859_15,
    ISO_8859_16,
    EBCDIC,
    SHIFT_JIS,
    EUC_JP,
    ISO_2022_JP,
    SHIFT_JIS-2004,
    EUC_JIS_2004,
    ISO_2022_JP_2004,
    GB_2312,
    GBK,
    GB_18030,
    HONG_KONG_HKSCS,
    KS_X_1001,
    EUC_KR,
    ISO_2022_KR,
}
```

All the supported encoding methods.

## Encode

```
export encode(string: String, method: Method): List<UInt8>;
```

Returns the text `string`, encoded as an array of bytes using the encoding method `method`.

## Decode

```
export decode(bytes: uint8[], method: Method): String;
```

Returns the array of bytes `bytes`, decoded as a string using the encoding method `method`.

```
int add(a, b) a + b;
```
