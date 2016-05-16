# txtelite
Erlang version of Ian Bell's "text Elite" (see link below). Also contains an updated version of the original C code.

I wrote the following text on my blog in February 2009:

This last weekend, I somehow came to think about the wonderful old space trading game Elite,
and the way it managed to cram a vast universe of planets with names, stats, and fun descriptions
(along with the 3D space flight simulation, which was an amazing feat in itself) into the tiny
8-bit home computers of old.

Only minutes later, I had found Ian Bell's <a href="http://www.iancgbell.clara.net/elite/text/">Text Elite</a>
web page, where Ian, one of the two original authors of the game, has published a reconstruction in C of the
main universe-generating algorithm, along with a simple text interface to actually let you "play",
i.e., buy, sell, and jump between planets. Great!

So I rewrote it in <a href="http://www.erlang.org/">Erlang</a>. And had a lot of fun doing it.
Apart from the nostalgic kick, there were two points with the exercise:
First of all, I just wanted to see the main algorithm as cleanly implemented as possible.
But second, I wanted to see what it would be like to rewrite a piece of classic game code that was
very clearly not written with functional programming in mind (though of course already cleaned up a bit
by being rewritten in C from the original 6502 assembler).

I think I like how the result turned out; it's quite clean and readable, and the data flows are easy to follow.
I also found and fixed a bug in the "goat soup" function.

The code can be found on <a href="https://github.com/richcarl/txtelite">GitHub</a>,
along with the two scripts for testing found on Ian's page. You can run them like this:
<blockquote>erl -noshell -s txtelite main -s init stop &lt; sinclair.txt</blockquote>
(To play yourself, just skip the last "< sinclair.txt" part.)

Of course, you need to compile the txtelite.erl file first. If you're new to Erlang, this is how:
<blockquote>erlc txtelite.erl</blockquote>

It's good to be back on Lave again!
