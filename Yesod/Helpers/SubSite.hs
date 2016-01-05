module Yesod.Helpers.SubSite where

import Prelude
import Yesod
import Yesod.Static                         (StaticRoute, Static)

class ToStaticRoute site where
    toStaticRoute :: StaticRoute -> Route site

instance ToStaticRoute Static where
    toStaticRoute = id


liftWithRouteToParent :: Monad m
                        => ((Route child -> Route parent) -> HandlerT parent m b)
                        -> HandlerT child (HandlerT parent m) b
liftWithRouteToParent f = getRouteToParent >>= lift . f
