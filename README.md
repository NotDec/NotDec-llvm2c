# NotDec LLVM IR to C

<!-- [**Click to Read the Book (TODO)**](TODO) -->

<!-- [**Online Demo**](TODO) -->

NotDec-LLVM2C is a book (TODO) and an example project that demonstrates the structuring algorithm in the decompiler.
- **Tutorial Book**: A series of tutorial articles is provided to better demonstrate the internals of the decompiler.
- **Clang Data Structure**: Use [Clang CFG](https://clang.llvm.org/doxygen/classclang_1_1CFG.html) as the main data structure and Translate to Clang AST.
- **Multiple Implementation**: Implements the [Phoenix](https://www.usenix.org/conference/usenixsecurity13/technical-sessions/presentation/schwartz) and [Dream](https://www.ndss-symposium.org/ndss2015/ndss-2015-programme/no-more-gotos-decompilation-using-pattern-independent-control-flow-structuring-and-semantics/) (WIP) algorithm.

Keywords: C LLVM Decompiler Reverse-engineering

Similar projects: 

- [lifting-bits/Rellic](https://github.com/lifting-bits/rellic)
- [llvm-cbe](https://github.com/JuliaHubOSS/llvm-cbe)

### We need your feedback!

- **Suggestions for the book**: It is hard to write without feedback!
  - If you cannot understand any part after reading the book, just [create a thread](https://github.com/NotDec/DecompilerWiki/discussions/new?category=write-a-decompiler-backend-from-scratch) in the GitHub discussion!
  - If you have suggestions for the book, feel free to create an issue! TODO: issue template and attach link.
- **Tool crash**: If the tool crashes on any LLVM IR, Please raise an issue with the IR.

## Examples

<table>
<thead>
  <td>Original program</td>
  <td>Compiled with <code>-disable-O0-optnone -emit-llvm -O0</code> and decompiled</td>
</thead>
<tbody>
<tr>
<td>

```c
int ifElseIf() {
  int a;
  a = 5;
  int b;
  b = 10;
  if(a == 6 || b == 0xb) {
    return a;
  }
  else {
    if (b == 10 && a == 1)
      a = 25;
    else if (b == 10 && a == -5)
      a = a + 15;
    else
      a = -+a;
  }
  return a;
}
```

</td>
<td>

```c
unsigned int ifElseIf() {
    unsigned int temp_0;
    unsigned int temp_1;
    unsigned int temp_2;
    temp_1 = 5U;
    temp_2 = 10U;
    if (temp_1 == 6U || temp_2 == 11U) {
        temp_0 = temp_1;
    } else {
        if (temp_2 == 10U && temp_1 == 1U) {
            temp_1 = 25U;
        } else {
            if (temp_2 == 10U && temp_1 == 4294967291U) {
                temp_1 = temp_1 + 15U;
            } else {
                temp_1 = 0U - temp_1;
            }
        }
        temp_0 = temp_1;
    }
    return temp_0;
}
```

</td>
</tr>

</tbody>
</table>


## Development

<!-- TODO: Add docker image. -->
