# Technical Paper on the Construction of Sigils

While historically the different types of sigils are well known, its construction is sometimes missing. Thus, we present a collection of different types of sigils and different possible methods for constructing them.

For each type of sigil we will first present its history, followed by an overview of the construction. We also include an opinionated list of positive and negative aspects that come with each type.

## History of Sigils

### Origin of Magical Symbols

Magical symbols, as known today, can be dated back to Arabic translations of Greek translations of Egyptian papyrus scrolls. While translating into Greek, the Greeks used letters of their own alphabet with added circles as a replacement for the magical Egyptian glyphs. While the Egyptian glyphs had a very specific meaning, the Greek magical symbols did not. Can can even see the same symbols be used by two different authors for two different meanings. (See [Magical Symbols - History of Sigils Angelic Alphabets and Symbols of Power by ESOTERICA](https://www.youtube.com/watch?v=7dhz4GR2yio))

<interactive  name="greekMagicSymbols">
_Font "Greek Magic" designed by Hermetic Mind in 2021. The graphics are licensed under [CC-BY-4.0](https://creativecommons.org/licenses/by/4.0/)_
</interactive>

In that sense these symbols are used similarly to the way how symbols are used in mathematics: You can declare that a specific symbols has a specific meaning and then continue using said symbol as a sort of abbreviation. This means that the symbol itself has no magical power, it just denotes a magical concept.

#### Positives

* One symbol can have multiple purposes.

#### Negatives

* The construction is based on an existing alphabet, therefore the amount of symbols per type of construction is limited by the amount of letters in the alphabet.

* The meaning of the symbols needs to be explicitly stated before it can be used. (The symbol on its own has no meaning)

### Early Sigils

In the Kabbalah we see new forms of magical symbols that are not related to Greek letters. They are composed out of straight lines with circles on their ends. It seems like they had or have a specific underlying construction that has been lost during translation. (See [Magical Symbols - History of Sigils Angelic Alphabets and Symbols of Power by ESOTERICA](https://www.youtube.com/watch?v=7dhz4GR2yio))

<image  title="Page taken from the Sefer Raziel HaMalakh"  src="https://upload.wikimedia.org/wikipedia/commons/2/2d/Sefer_raziel_segulot.png"  height="240"></image>

### Sigils by Austin Osman Spare

Over time the methods for constructing sigils got more and more corrupted. An artist and occultist called Austin Osman Spare therefore came up with a simpler construction:  First remove any multiple letters of the word, then write down the letters one by one, such that they form a unique glyph. Additionally one usually also arbitrarily rotates the canvas after each letter.

<image  title="Example of a sigil, taken from the wikipedia page on sigils"  src="https://upload.wikimedia.org/wikipedia/commons/b/b5/Sigil.svg"  height="120"></image>

#### Positives

* The creation of the sigil is on its own a ritual.
* The end result depends strongly on the person creating the sigil.

#### Negatives

* The construction is heuristic.
* The resulting sigils are very personal. The creation is dependent on various factors like who is creating it, in which state/mood that they are in and what is happening around them. To underline this, Austin Osman Spare himself once said that you can obtain the best results while being drunk. (See [AOS to Zos: The Life and Art of Austin Osman Spare by Queer Amalgams](https://www.youtube.com/watch?v=Kh3W0QLhYZo))

## Construction

### Construction using Attributes

One way for constructing such sigils is by first spacially arranging concepts and then connecting the one that the sigil should represent. As an example we can construct symbols for the four elements by connecting the respective qualities: Hot, cold, dry and wet (See [Aristotilean Qualities of Elements](https://en.wikipedia.org/wiki/Classical_element#Greece))

<image  title="Sigils for the four elements based on their qualities."  src="http://hermeticMind.github.io/attributeSigil.png"  height="240"></image>

Note that we use binary attributes (Hot/Cold and Dry/Wet) for the construction of the elements. Thus, the sigils created by "hot" and "Cold" or by "Dry" and "Wet" are not valid constructions.

This motivates the mapping of the element onto the numbers 0 to 3 in binary presentation: Water - 00, Earth - 01, Fire - 10, Wind - 11. You might wonder why one starts with water and then continues clockwise. One can map the elements on the four seasons: Water - Winter, Earth - Spring, Fire - Summer, Wind - Fall and then use the order of the seasons. Winter is the first of the four seasons (according to our calendar).

<interactive  name="BinarySigil">
_Designed by Hermetic Mind by extending on the original construction. The graphics are licensed under [CC-BY-4.0](https://creativecommons.org/licenses/by/4.0/)._
</interactive>

#### Positives

* Allows the construction of a family of sigils, all at once.
* It is highly scalable.
* The construction may still vary, but anyone knowing the construction methods can reproduce the results.

#### Negatives

* Needs a set of attributes
* May produce a few "invalid" sigils as a side product. (Like "Hot~Cold" or "Dry~Wet" from our example)
* The size of the sigils increases (linearly) with the number of attributes.

### Construction using Magic Squares

A common way for medieval sigils was to use a magic square. (See [Magic Squares](https://en.wikipedia.org/wiki/Magic_square)) One first transforms the letters of a word into numbers. (See [Gematria](https://en.wikipedia.org/wiki/Gematria) for transforming jewish words) Next one connects the corresponding fields in the magic square, resulting in a symbol. Typically, a magic square of degree 3 is used. By computing the remainder of the division by 9 one can get a mapping from the alphabet to the fields of the square.

<interactive  name="MagicSquareSigil"  value="Hermetic Mind">
_Designed by Hermetic Mind using a magic square of degree 6 in order to avoid the dividing process. Additionally, the alphabet gets ordered by the amount of occurrences in the english dictionary and shifted by 7 (a prim not dividing 26). The graphics are licensed under [CC-BY-4.0](https://creativecommons.org/licenses/by/4.0/)._
</interactive>

We will be using a magic square of degree 6 and permutate the alphabet according to the occurrences in the english dictionary. We do this to obtain better results.

<interactive  name="MagicSquareSigil"  value="Banana">
</interactive>

If we want to encode a value with repeating patterns like "banana", this method will loose information. One might want to add, that this is not necessarily a bad thing, it is just something one needs to keep in mind, when choosing a method for constructing sigils.

<interactive  name="MagicSquareSigil"  value="Bana">
</interactive>

#### Positives

* Very explicitly defined construction with not much space for interpretation.
* Can be used to encode words or even sentences. (No size increase!)

#### Negatives

* Even if one uses a magic square of degree 6 or higher, one looses information. In practice this should not be noticeable.

### Construction using a Circle

Now lets got back and try to construct new sigils for the four element, but this time we consider the binary numbers as points on the circle. Two same numbers results in a spiral (circling around the point), Two different numbers result in a line.

<image  title="The four elements represented as a sigil constructed by circle."  src="http://hermeticMind.github.io/circleSigil.png"  height="240"></image>

This concept can be extended to three points and also even further to 26 points (the letters of the alphabet).

Remark: It does not matter if the spiral goes clockwise or counterclockwise. The only important distinction is if the spiral starts left or right of the vertical line.

<interactive  name="BraidSigil"  value="Hermetic Mind">
_Designed by Hermetic Mind as described above. The alphabet gets arranged around the circle with respect to the amount of occurrences in the English dictionary. Additionally we do a shift by 7 (a prim not dividing 26). The end point gets connected with the starting point to produce a looping path. The graphics are licensed under [CC-BY-4.0](https://creativecommons.org/licenses/by/4.0/)._
</interactive>

#### Positives

* Very explicitly defined construction with not much space for interpretation.
* Can be used to encode words. (Small size increase)
* No Information loss.

#### Negatives

* The circle radius needs to grow with the input, thus full sentences may produce unwanted results. In practice about 50 letters should work nicely.
