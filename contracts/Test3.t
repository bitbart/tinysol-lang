// unit testing Test3.sol
faucet 0xA 200
faucet 0xB 100

deploy 0xA:0xD{value:100}("0xC") "D" "contracts/Test3.sol"
assert 0xA this.balance==100
assert 0xD this.balance==100
deploy 0xA:0xC("0xD") "C" "contracts/Test3.sol"

0xA:0xD.foo()

assert 0xA this.balance==100
assert 0xB this.balance==100
assert 0xC this.balance==0
assert 0xD this.balance==100
assert 0xD x==0