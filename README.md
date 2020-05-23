# CSS normalize

CSS normalize is a tool for normalizing CSS files.
It was developed to facilitate comparing CSS files using text comparison tools such as diff.
It takes CSS stylesheets as input and outputs a semantically equivalent normalized stylesheet.
The aforementioned equivalence means that the output file might e.g. have a different order of style rules but adding or removing rules is prohibited.
The output file is normalized or canonical in the sense that running the tool on an already normalized stylesheet should produce output identical to the input.
In other words the process of normalization is idempotent.

Transformations applied include:

- sorting properties by key within a style rule,
- sorting selectors in a group of comma separated selectors,
- adding leading zeros to floating point numeric literals (e.g changing `.5` to `0.5`)
- applying uniform formatting

## Building

```
stack build
```

## Usage

You can specify the input file by passing its' name as the first positional parameter.

```
cssn raw.css > normalized.css
```

To compare two css files first normalize both with cssn

```
cssn a.css > a.norm.css
cssn b.css > b.norm.css
```

Then compare the normalized files using a standart text comparison tool such as diff

```
diff a.norm.css b.norm.css
```
