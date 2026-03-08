module WebDriverPreCore.Extended.Locators
  ( -- * Locator Type
    Locator,

    -- * Re-exports from Tags
    module WebDriverPreCore.Extended.Tags,

    -- * Re-exports from Internal
    AriaRole (..),
    MatchType (..),
    displayAriaRole,

    -- * Smart Constructors
    css,
    role',
    role,

    -- * Role-specific Smart Constructors
    article,
    banner,
    button,
    cell,
    checkbox,
    columnHeader,
    complementary,
    contentInfo,
    definition,
    dialog,
    figure,
    form,
    group,
    heading,
    img,
    link,
    list,
    listItem,
    mainRole,
    navigation,
    option,
    progressBar,
    radio,
    region,
    row,
    rowHeader,
    search,
    separator,
    slider,
    spinButton,
    status,
    table,
    term,
    textbox,

    -- * Combinators
    (&&&),
    (|||),
    (>>>),
    notLoc
  )
where

import Data.List.NonEmpty (NonEmpty (..), toList)
import Data.Text (Text)
import WebDriverPreCore.Extended.Locators.Internal
import WebDriverPreCore.Extended.Tags
import Prelude

css :: Text -> Locator
css = CSS

------- Role Smart Constructors -------

role' :: Maybe AriaRole -> Maybe Text -> Locator
role' r = Role r

role :: AriaRole -> Text -> Locator
role r = role' (Just r) . Just

article :: Text -> Locator
article = role Article

banner :: Text -> Locator
banner = role Banner

button :: Text -> Locator
button = role Button

cell :: Text -> Locator
cell = role Cell

checkbox :: Text -> Locator
checkbox = role Checkbox

columnHeader :: Text -> Locator
columnHeader = role ColumnHeader

complementary :: Text -> Locator
complementary = role Complementary

contentInfo :: Text -> Locator
contentInfo = role ContentInfo

definition :: Text -> Locator
definition = role Definition

dialog :: Text -> Locator
dialog = role Dialog

figure :: Text -> Locator
figure = role Figure

form :: Text -> Locator
form = role Form

group :: Text -> Locator
group = role Group

heading :: Text -> Locator
heading = role Heading

img :: Text -> Locator
img = role Img

link :: Text -> Locator
link = role Link

list :: Text -> Locator
list = role List

listItem :: Text -> Locator
listItem = role ListItem

mainRole :: Text -> Locator
mainRole = role Main

navigation :: Text -> Locator
navigation = role Navigation

option :: Text -> Locator
option = role Option

progressBar :: Text -> Locator
progressBar = role ProgressBar

radio :: Text -> Locator
radio = role Radio

region :: Text -> Locator
region = role Region

row :: Text -> Locator
row = role Row

rowHeader :: Text -> Locator
rowHeader = role RowHeader

search :: Text -> Locator
search = role Search

separator :: Text -> Locator
separator = role Separator

slider :: Text -> Locator
slider = role Slider

spinButton :: Text -> Locator
spinButton = role SpinButton

status :: Text -> Locator
status = role Status

table :: Text -> Locator
table = role Table

term :: Text -> Locator
term = role Term

textbox :: Text -> Locator
textbox = role Textbox

(&&&) :: Locator -> Locator -> Locator
l &&& r = And (l :| [r])

infixr 3 &&&

(|||) :: Locator -> Locator -> Locator
l ||| r = Or (l :| [r])

infixr 2 |||

(>>>) :: Locator -> Locator -> Locator
(>>>) = Parent

infixr 1 >>>


notLoc :: Locator -> Locator
notLoc l = Not (l :| [])

