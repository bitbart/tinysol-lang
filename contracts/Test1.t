// unit testing Test1.sol
faucet 0xA 200
faucet 0xB 100

deploy 0xA:0xD{value:100}() "D" "contracts/Test1.sol"
assert 0xD this.balance==100
assert 0xA this.balance==100
deploy 0xA:0xC("0xD") "C" "contracts/Test1.sol"

assert 0xB this.balance==100
assert 0xC this.balance==0
assert 0xD this.balance==100

0xA:0xD.f("0xC")

assert 0xC this.balance==100
assert 0xD this.balance==0

0xB:0xC.foo("0xA")

assert 0xA this.balance==110
assert 0xB this.balance==100
assert 0xC this.balance==90
assert 0xD this.balance==0
assert 0xD x==1