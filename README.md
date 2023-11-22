# my_PGP

**my_PGP** is a cryptographic encryption solution who use xor, rsa and aes to encrypt message

## How does it work?

**my_PGP** can be used for different purpose:

- xor messsage
- generate rsa Keys
- encrypt message with rsa keys
- encrypt message with aes
- encrypt message with pgp (rsa + aes)

## Getting Started

### Installation

Install stack:

`
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh &&
ghcup install stack
`

Build the project:

`
make
`

### Usage

```
./mypgp -h
USAGE
    ./mypgp [-xor | -aes | -rsa | -pgp] [-c | -d] [-b] KEY
    the MESSAGE is read from standard input
DESCRIPTION
    -xor computation using XOR algorithm
    -aes computation using AES algorithm
    -rsa computation using RSA algorithm
    -pgp computation using both RSA and AES algorithm
    -c MESSAGE is clear and we want to cipher it
    -d MESSAGE is ciphered and we want to decipher it
    -b block mode: for xor and aes, only works on one block
    MESSAGE and KEY must be of the same size
    -g P Q for RSA only: generate a public and private key
    pair from the prime number P and Q
```