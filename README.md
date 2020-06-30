# feederiken ![CI](https://github.com/feederiken/feederiken/workflows/CI/badge.svg)
Generate PGP keys with custom patterns in the fingerprint.

Experimental support for distributed operation is available.

## Obtaining
Make sure you have Java installed.
Grab the self contained jar from the releases tab.

Alternatively, build from source using [mill](https://www.lihaoyi.com/mill).

## Usage
Use the `search` command to look for keys with a given prefix.
The default setting should take only a few seconds, so try that first.
Note that the time required to find a matching key increases exponentially with the length of the required prefix, see the next section for more details.

## Remarks
The fingerprint of a key is determined using a [hash function](https://en.wikipedia.org/wiki/Cryptographic_hash_function),
as a result, the best method to get a matching key is to generate a key, compute its fingerprint, and throw it away if it doesn't match until eventually we find one by chance.
As a consequence, there is no notion of progress that can be saved and restored on the way to finding a key, it's just a matter of chance and each attempt is just as likely to succeed as any other one.
Additionally, to parallelize the search, you don't need any coordination, it's just a matter of running it until one computer finds it by chance.
