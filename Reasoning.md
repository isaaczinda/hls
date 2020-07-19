# How many bits should '0' take up?

We could build a system which stores the value 0 in a `UInt0`. The advantage of doing this is if you write `1 + 0`, since you're doing a `UInt1 + UInt0` operation, the resulting type still `UInt1` as it should be; adding zero doesn't force you to waste space.  

However, a statement like the following would also become confusing:

```
Int3 a = 0
```

If 0 is an `Int0`, then we have to sign extend to store it in an `Int3`. But an `Int0` contains no bits! So we will have to build in special cases all over in our logic to handle the `Int0`. It may just be better to use the 1 bit of space for ease of programming.

A system which stores the value 0 in a `UInt1` would be more like what people might expect, since you don't expect to have a variable actually does no storage.

**0 should be stored in an Int1**


# Why should fractional bits in Fixed stop at 32?

The size of the fractional component of a fixed point number needs to stop somewhere, because there are decimal numbers which can't be represented in binary at all. These numbers have infinite binary fractions. We chose 32 because anything larger would add additional hardware for hardly any precision gain (32 bits can represent any 10-digit fraction). If precision is important, it would be best to use a binary data type and then cast it to a Fixed.

# Implicit Casting

Implicit casting is supported so that operations such as + which only combine variables of the same type can be easily extended to work on variables of different types. For example, `1 (a UInt1) + -1 (a Int1)` should still work.

Implicit casting is performed as little as possible so that values are compatible and can be arguments to the `+` function. In this case, we will convert UInt1 to an Int2. 

# TODO
 * develop a better `fractionBits` function which works for large decimals
