
{ nixpkgs ? import <nixpkgs> {} }:
    import ./chc-core { inherit nixpkgs; }
