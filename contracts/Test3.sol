// SPDX-License-Identifier: MIT
pragma solidity >= 0.8.2;

/*
    Simulazione della Call Stack Depth Limit, in situazioni di loop infiniti effettua revert
*/

contract C {
    address d;

    constructor(address payable add_) { d = add_; }

    receive() external payable { payable(d).transfer(25); }
}

contract D {
    address c;
    int x;

    constructor(address payable add_) payable { c = add_; }

    function foo() public { payable(c).transfer(25); }

    receive() external payable { payable(c).transfer(25); }
}