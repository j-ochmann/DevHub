## Podmínka `if`

Podmíněné vykonání kódu při splnění podmínky.

### ABAP
```ABAP
REPORT ZEXAMPLE.

  DATA x TYPE i VALUE 0.

  IF X > 0.
    WRITE X.
  ENDIF.
```

### COBOL
```COBOL
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

### C++
```C++
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

### C#
```C#
if (x > 0)
{
    Console.WriteLine(x);
}
```

### Python
```Python
if x > 0:
    print(x)
```

### Fortran
```Fortran
      PROGRAM EXAMPLE
  IF (X > 0) THEN
    PRINT *, X
  ENDIF
      END
```
