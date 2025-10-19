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

1. Help download and install fonts files specified by users.
2. Allow user to quickly switch between different font profiles.
3. Adopting font profiles in several patterns. For example, using a
   different font profile by date, session, etc..
   
The package is simply named as `font-manager`.

# Defining Fonts

To use this package, we have to first define several fonts that we
intend to use. A font is an object which contains the following
fields:

+ `name`: the name of the font.
+ `url`: the URL to download the font files, it must point to a `.zip`
  file.

Register a new font with the following function
`font-manager-register-font`.

# Installing Fonts

To install fonts we have to download the font files and install them
to the operating system. Since this package is intended to work on
different operating systems, we have to ask the user to install the
font files manually. The package will handle the downloading and
extracting `.ttf` files.

# Defining Font Profiles

A font profile consists of the following properties:

+ `name`: the name of the profile.
+ `fixed-pitch-font`: font for code.
+ `variable-pitch-font`:  font for text fore prose.
+  `weight`: regular, bold, thin, etc..
+  `height`: affect the size of the font.

To register a new font profile, use
`font-manager-register-font-profile`.

# Applying Font Profiles

Applying a font profile applies its properties in Emacs.
