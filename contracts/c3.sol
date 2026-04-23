//SPDX-License-Identifier: UNLICENSED
pragma solidity <= 0.8;

contract C {
    function g() public returns(uint) { return 1; }
}

contract C3 {
    uint x;
    C c;

    // test parsing of function declarations and function calls
    
    function f0() public { }
    function f1() public { x = c.g(); }   
    function f2() public { x = c.g{value:1+2}(1,true); }   
    function f3() public { x = c.g{value:x}(1+1); }   
    function f4() public { return x; }   
    function f5() public { if (1+1>1) return x; else return 2; }   
    function f5() public { if (1+1>1) { return x; return 1; } else return 2; }   
    function f6() public { return c.g(); }   
    function f7() public { c.g{value:1}(); }
    function f8() public { if (c.g()) c.g{value:1}(); else c.g(); }
    function f9() public returns (uint) { return 1; }   
    function f10() public returns(int) { }
    function f11() public returns(address) { skip; }
    function f12() public returns(address payable) { skip; }
}