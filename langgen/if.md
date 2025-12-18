## Podmínka `if`

Podmíněné vykonání kódu při splnění podmínky.

### abap
```abap
REPORT ZEXAMPLE.

  DATA x TYPE i VALUE 0.

  IF X > 0.
    WRITE X.
  ENDIF.
```

### cobol85
```cobol85
IDENTIFICATION DIVISION.
PROGRAM-ID. EXAMPLE.

DATA DIVISION.
WORKING-STORAGE SECTION.
  01 X        PIC 9(4) VALUE 0.

PROCEDURE DIVISION.
  IF X > 0 THEN
    DISPLAY X.
  END-IF
  STOP RUN.
```

### cpp
```cpp
#include <iostream>

int main()
{
    if (x > 0)
    {
        std::cout << x << std::endl;
    }

    return 0;
}
```

### csharp
```csharp
if (x > 0)
{
    Console.WriteLine(x);
}
```

### python
```python
if x > 0:
    print(x)
```

### fortran77
```fortran77
      PROGRAM EXAMPLE
  IF (X > 0) THEN
    PRINT *, X
  ENDIF
      END
```

