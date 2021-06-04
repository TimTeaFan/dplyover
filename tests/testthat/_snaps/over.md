# over() gives meaningful messages

    `over()` must only be used inside dplyr verbs.

---

    Problem with `summarise()` input `..1`.
    i `..1 = over(1, 42)`.
    x Problem with `over()` input `.fns`.
    i Input `.fns` must be a function, a formula, or a list of functions/formulas.

---

    Problem with `summarise()` input `..1`.
    i `..1 = over(...)`.
    x Problem with `over()`  input `.names`.
    i The number of elements in `.names` must equal the number of new columns.
    x 3 elements provided to `.names`, but the number of new columns is 6.
    i The error occurred in group 1: x = 1.

---

    Problem with `summarise()` input `..1`.
    i `..1 = over(...)`.
    x Problem with `over()`  input `.names`.
    i The number of elements in `.names` must equal the number of new columns.
    x 7 elements provided to `.names`, but the number of new columns is 6.
    i The error occurred in group 1: x = 1.

---

    Problem with `summarise()` input `..1`.
    i `..1 = over(...)`.
    x Names must be unique.
    x These names are duplicated:
      * "one" at locations 1 and 4.
      * "two" at locations 2 and 5.
      * "three" at locations 3 and 6.
    i The error occurred in group 1: x = 1.

