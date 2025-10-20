This is a package derived from an aggregation of my personal code
managing fonts in Emacs. I enjoy beautiful fonts and there are lots of
them around the world. To name a few, Cascadia Code, JetBrains Mono,
Aporetic... I wanted to settle down with one of them, but I found that
even the fonts I appreciated the most would make me exhaustive after
seeing them months after months. Thus, the idea of quickly switching
different fonts come to my mind. It might be difficult to switch
between multiple mates but it should not be difficult to switch
between multiple fonts. In fact, there is a similar package out there,
the [Fontaine](https://protesilaos.com/emacs/fontaine) by
Protesilaos. However, its vision is a little different from my own. It
features customizing different faces so that you can use different
fonts for code, text, mode line, header line, etc.. Though it does
provide means to switch between different profiles, but that is not
its focus. Using certain fonts persistently for a longer time is still
implicitly implied by that package. Though I can add some customized
code to make it suit for my purpose, I decide to put **my own code**
into a package. It might not used by the others, but I would be happy
with it.

The vision of the package is as follows:

1. Downloading fonts files specified by users.
2. Switching between different profiles.
3. Adopting profiles in several patterns. For example, using a
   different profile by date,  randomly, etc..
   
The package is simply named as `foma` for FOnt MAnagement.

# Font

A font in `foma` is simply a pair whose `car` is its name and `cdr` is
an URL that points to a `.zip` file that contains the `.ttf` font
files. Register fonts to `foma` by adding its pair to `foma-fonts`.

# Downloading Font Files

`foma-download-all-fonts` iterate through `foma-fonts` and download
every `.zip` files specified by the URL in each pair. After that, the
`.ttf` files would be extracted from the `.zip` files in the same
directory for the users to install manually.

I am planning to support downloading font files from Google Fonts, but
this requires an API key which has to be provided by the user. This
can be done by using Google Fonts web API.

Example:

https://www.googleapis.com/webfonts/v1/webfonts?key=your-api-key&family=noto+sans

Replace `your-api-key` with the actual API key. This query would
return the metadata of Noto Sans as follows:

```json
{
  "kind": "webfonts#webfontList",
  "items": [
    {
      "family": "Noto Sans",
      "variants": [
        "100",
        "200",
        "300",
        "regular",
        "500",
        "600",
        "700",
        "800",
        "900",
        "100italic",
        "200italic",
        "300italic",
        "italic",
        "500italic",
        "600italic",
        "700italic",
        "800italic",
        "900italic"
      ],
      "subsets": [
        "cyrillic",
        "cyrillic-ext",
        "devanagari",
        "greek",
        "greek-ext",
        "latin",
        "latin-ext",
        "vietnamese"
      ],
      "version": "v42",
      "lastModified": "2025-09-11",
      "files": {
        "100": "https://fonts.gstatic.com/s/notosans/v42/o-0mIpQlx3QUlC5A4PNB6Ryti20_6n1iPHjcz6L1SoM-jCpoiyD9At9d41P6zHtY.ttf",
        "200": "https://fonts.gstatic.com/s/notosans/v42/o-0mIpQlx3QUlC5A4PNB6Ryti20_6n1iPHjcz6L1SoM-jCpoiyB9A99d41P6zHtY.ttf",
        "300": "https://fonts.gstatic.com/s/notosans/v42/o-0mIpQlx3QUlC5A4PNB6Ryti20_6n1iPHjcz6L1SoM-jCpoiyCjA99d41P6zHtY.ttf",
        "regular": "https://fonts.gstatic.com/s/notosans/v42/o-0mIpQlx3QUlC5A4PNB6Ryti20_6n1iPHjcz6L1SoM-jCpoiyD9A99d41P6zHtY.ttf",
        "500": "https://fonts.gstatic.com/s/notosans/v42/o-0mIpQlx3QUlC5A4PNB6Ryti20_6n1iPHjcz6L1SoM-jCpoiyDPA99d41P6zHtY.ttf",
        "600": "https://fonts.gstatic.com/s/notosans/v42/o-0mIpQlx3QUlC5A4PNB6Ryti20_6n1iPHjcz6L1SoM-jCpoiyAjBN9d41P6zHtY.ttf",
        "700": "https://fonts.gstatic.com/s/notosans/v42/o-0mIpQlx3QUlC5A4PNB6Ryti20_6n1iPHjcz6L1SoM-jCpoiyAaBN9d41P6zHtY.ttf",
        "800": "https://fonts.gstatic.com/s/notosans/v42/o-0mIpQlx3QUlC5A4PNB6Ryti20_6n1iPHjcz6L1SoM-jCpoiyB9BN9d41P6zHtY.ttf",
        "900": "https://fonts.gstatic.com/s/notosans/v42/o-0mIpQlx3QUlC5A4PNB6Ryti20_6n1iPHjcz6L1SoM-jCpoiyBUBN9d41P6zHtY.ttf",
        "100italic": "https://fonts.gstatic.com/s/notosans/v42/o-0kIpQlx3QUlC5A4PNr4C5OaxRsfNNlKbCePevHtVtX57DGjDU1QDcf6VfYyWtY1rI.ttf",
        "200italic": "https://fonts.gstatic.com/s/notosans/v42/o-0kIpQlx3QUlC5A4PNr4C5OaxRsfNNlKbCePevHtVtX57DGjDU1QLce6VfYyWtY1rI.ttf",
        "300italic": "https://fonts.gstatic.com/s/notosans/v42/o-0kIpQlx3QUlC5A4PNr4C5OaxRsfNNlKbCePevHtVtX57DGjDU1QGke6VfYyWtY1rI.ttf",
        "italic": "https://fonts.gstatic.com/s/notosans/v42/o-0kIpQlx3QUlC5A4PNr4C5OaxRsfNNlKbCePevHtVtX57DGjDU1QDce6VfYyWtY1rI.ttf",
        "500italic": "https://fonts.gstatic.com/s/notosans/v42/o-0kIpQlx3QUlC5A4PNr4C5OaxRsfNNlKbCePevHtVtX57DGjDU1QAUe6VfYyWtY1rI.ttf",
        "600italic": "https://fonts.gstatic.com/s/notosans/v42/o-0kIpQlx3QUlC5A4PNr4C5OaxRsfNNlKbCePevHtVtX57DGjDU1QOkZ6VfYyWtY1rI.ttf",
        "700italic": "https://fonts.gstatic.com/s/notosans/v42/o-0kIpQlx3QUlC5A4PNr4C5OaxRsfNNlKbCePevHtVtX57DGjDU1QNAZ6VfYyWtY1rI.ttf",
        "800italic": "https://fonts.gstatic.com/s/notosans/v42/o-0kIpQlx3QUlC5A4PNr4C5OaxRsfNNlKbCePevHtVtX57DGjDU1QLcZ6VfYyWtY1rI.ttf",
        "900italic": "https://fonts.gstatic.com/s/notosans/v42/o-0kIpQlx3QUlC5A4PNr4C5OaxRsfNNlKbCePevHtVtX57DGjDU1QJ4Z6VfYyWtY1rI.ttf"
      },
      "category": "sans-serif",
      "kind": "webfonts#webfont",
      "menu": "https://fonts.gstatic.com/s/notosans/v42/o-0mIpQlx3QUlC5A4PNB6Ryti20_6n1iPHjcz6L1SoM-jCpoiyD9A-9c6Vc.ttf"
    }
  ]
}
```

Then we can use the link to `.ttf` files in the metadata to download
`.ttf` files directly. See the Google Fonts API
[documentation](https://developers.google.com/fonts/docs/developer_api)
for more information.

# Profiles

A profile is a list that contains the following elements:

+ `name`: the name of the profile.
+ `fixed-pitch-font`: font for code.
+ `variable-pitch-font`:  font for text.
+  `weight`: regular, bold, thin, etc..
+  `height`: determine the size of the font.

Profiles are stored in `foma-profiles`.

# Applying Profiles

Applying a profile applies its properties in Emacs. Use
`foma-apply-profile` and helper functions `foma-apply-profile-rand`
and `foma-apply-profile-by-day`.
