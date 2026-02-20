// SPDX-License-Identifier: MIT
pragma solidity >= 0.8.2;

/* 
    Test chiamata a funzione attivata a catena dal corpo della receive
*/

contract C { 
    address d;
    constructor(address payable add_) { d = add_; }
    function foo(address add_) public { payable(add_).transfer(10); }
    receive() external payable {
        d.g();
    }
}

contract D { 
    uint x;
    constructor() payable { }
    function f(address a_) public { payable(a_).transfer(100); }
    function g() public { x = 1; }
    receive() external payable { x += 1; }
}