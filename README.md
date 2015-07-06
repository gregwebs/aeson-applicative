make To/From JSON instances from an applicative description

    data R = R { a :: Text, b :: Int32 } deriving (Typeable)
   
    rJsonInfo :: JsonInfo R
    rJsonInfo = R
            <$> "a" @: a
            <*> 'b  @: b
   
    instance ToJSON R where
      toJSON = jiToJSON rJsonInfo
    instance FromJSON R where
      parseJSON = jiParseJSON rJsonInfo


This is a simple application of a [bi-directional applicative DSL concept](http://jaspervdj.be/posts/2012-09-07-applicative-bidirectional-serialization-combinators.html).

It has the advantage of not requiring Template Haskell.
However, the resulting code will certainly be less efficient.
A faster implementation would be to use Template Haskell to generate efficient code.
