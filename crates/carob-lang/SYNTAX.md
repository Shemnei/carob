# Syntax

## Comment

Every line begining with a special character (NOT a-zA-Z0-9) will be interpreted
as line comment.

```text
; This is a line comment
```

## Option

```text
option "OPTION_NAME" "OPTION_VALUE"
```

## Include

```text
include "RELATIVE_INCLUDE_PATH"
```

## Dated entry

```text
yyyy-mm-dd ENTRY_KIND ...
```

## Tag

```text
#TAG_NAME
```

## Link

```text
^LINK_NAME
```

## Attribute

```text
ATTRIBUTE_NAME: "ATTRIBUTE_VALUE"
```
