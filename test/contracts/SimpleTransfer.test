// unit testing SimpleTransfer.sol

faucet 0xA 100
faucet 0xM 10
faucet 0xB 0

deploy 0xA:0xC("0xB") "test/contracts/SimpleTransfer.sol"
assert 0xC this.balance==0
assert 0xA this.balance==100

0xA:0xC.deposit{value : 10}()
assert 0xC this.balance==10
assert 0xA this.balance==90

0xM:0xC.deposit{value : 10}()
assert 0xC this.balance==10
assert 0xA this.balance==90

revert 0xA:0xC.withdraw{value : 10}(3)

revert 0xA:0xC.withdraw(3)

0xB:0xC.withdraw(3)
assert 0xC this.balance==7
assert 0xA this.balance==90
assert 0xB this.balance==3

revert 0xB:0xC.withdraw(7)

0xB:0xC.withdraw(6)

assert 0xC this.balance==1
assert 0xA this.balance==90
assert 0xB this.balance==9

revert 0xA:0xC.withdraw(8)

revert 0xB:0xC.withdraw(1)
