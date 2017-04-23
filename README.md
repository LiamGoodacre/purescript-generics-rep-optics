# purescript-generics-rep-optics

[![Build status](https://travis-ci.org/LiamGoodacre/purescript-generics-rep-optics.svg?branch=master)](https://travis-ci.org/LiamGoodacre/purescript-generics-rep-optics)

Optics for generics rep.

```purescript
-- given the following data types
data I = I String
data E = L Int | R Boolean
derive instance genericI :: Generic I _
derive instance genericE :: Generic E _
_I = SProxy :: SProxy "I"
_L = SProxy :: SProxy "L"
_R = SProxy :: SProxy "R"

-- the following optics exist
ctor _I :: Iso' I String
ctor _L :: Prism' E Int
ctor _R :: Prism' E Boolean
```

Check out [test/Main](test/Main.purs) for more examples.

## Installation

```
bower install purescript-generics-rep-optics
```

## Documentation

See on [Pursuit](https://pursuit.purescript.org/packages/purescript-generics-rep-optics/).

Or build docs with `npm run build:docs`.
