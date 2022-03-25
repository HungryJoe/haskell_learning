fmapList :: [a] -> (a -> b) -> [b]
fmapList [] _ = []
fmapList (x:xs) f = f x : fmapList xs f

fmapMaybe :: Maybe a -> (a -> b) -> Maybe b
fmapMaybe Nothing _ = Nothing
fmapMaybe (Just x) f = Just (f x)

applicativeMaybe :: Maybe (a -> b) -> Maybe a -> Maybe b
applicativeMaybe Nothing _ = Nothing
applicativeMaybe _ Nothing = Nothing
applicativeMaybe (Just f) (Just x) = Just (f x)

applicativeList :: [a -> b] -> [a] -> [b]
applicativeList [] _ = []
applicativeList _ [] = []
applicativeList (f:fs) xs = fmap f xs ++ applicativeList fs xs

bindMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
bindMaybe Nothing _ = Nothing
bindMaybe (Just x) f = f x

bindList :: [a] -> (a -> [b]) -> [b]
bindList [] _ = []
bindList (x:xs) f = f x ++ bindList xs f
