// SPDX-License-Identifier: MIT
pragma solidity >= 0.8.2;

/*
    Test revert a stato iniziale dopo fallimento require nel corpo della receive()
*/

contract C {
    address d;

    constructor(address payable add_) { d = add_; }

    receive() external payable { payable(d).transfer(10); }
}

contract D {
    address c;
    int x;

    constructor (address payable add_) payable { c = add_; }

    function foo() public {
        payable(c).transfer(50);
        x += 1;
    }

    receive() external payable {
        require(msg.value > 10);
        x += 10;
    }
}