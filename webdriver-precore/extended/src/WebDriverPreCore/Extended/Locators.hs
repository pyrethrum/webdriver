module WebDriverPreCore.Extended.Locators
  ( -- * Locator Type
    Locator,

    -- * Re-exports from Internal
    AriaRole (..),
    MatchType (..),
    displayAriaRole,

    -- * Smart Constructors
    css,
    xpath,
    defaultId,
    --
    allElms,
    elmId,
    --
    elmClass,
    elmClass',
    elmClassExact,
    elemClassStarts,
    --
    attribute,
    attribute',
    attributeExact,
    attributeStarts,
    -- 
    -- post filters
    value,
    valueExact,
    value',
    valueStarts,
    valueFunc,

    -- * Role Constructors
    role,
    role',
    --
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

    -- * Tag Constructors
    customTag,
    a_,
    abbr_,
    address_,
    area_,
    article_,
    aside_,
    audio_,
    b_,
    base_,
    bdi_,
    bdo_,
    blockquote_,
    body_,
    br_,
    button_,
    canvas_,
    caption_,
    cite_,
    code_,
    col_,
    colgroup_,
    data_,
    datalist_,
    dd_,
    del_,
    details_,
    dfn_,
    dialog_,
    div_,
    dl_,
    dt_,
    em_,
    embed_,
    fieldset_,
    figcaption_,
    figure_,
    footer_,
    form_,
    h1_,
    h2_,
    h3_,
    h4_,
    h5_,
    h6_,
    head_,
    header_,
    hgroup_,
    hr_,
    html_,
    i_,
    iframe_,
    img_,
    input_,
    ins_,
    kbd_,
    label_,
    legend_,
    li_,
    link_,
    main_,
    map_,
    mark_,
    menu_,
    meta_,
    meter_,
    nav_,
    noscript_,
    object_,
    ol_,
    optgroup_,
    option_,
    output_,
    p_,
    picture_,
    pre_,
    progress_,
    q_,
    rp_,
    rt_,
    ruby_,
    s_,
    samp_,
    script_,
    search_,
    section_,
    select_,
    slot_,
    small_,
    source_,
    span_,
    strong_,
    style_,
    sub_,
    summary_,
    sup_,
    table_,
    tbody_,
    td_,
    template_,
    textarea_,
    tfoot_,
    th_,
    thead_,
    time_,
    title_,
    tr_,
    track_,
    u_,
    ul_,
    var_,
    video_,
    wbr_,

    -- * Combinators
    (&&&),
    (|||),
    (>>>),
    notLoc,
  )
where

import Data.Bool (bool)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text, isInfixOf)
import WebDriverPreCore.Extended.Locators.Internal
import Prelude

css :: Text -> Locator
css = CSS

xpath :: Text -> Locator
xpath = XPath

defaultId :: Text -> Locator
defaultId = Default

allElms :: Locator
allElms = AllElms

elmId :: Text -> Locator
elmId = ID

deriveMatch :: Text -> MatchType
deriveMatch = bool Partial Wildcard . ("*" `isInfixOf`)

mkPassThrough :: (Text -> MatchType -> CaseSensitivity -> Locator) -> MatchType -> CaseSensitivity -> Text -> Locator
mkPassThrough constructor mt cs v = constructor v mt cs

mkDefaults :: (MatchType -> CaseSensitivity -> Text -> Locator) -> Text -> Locator
mkDefaults constructor val = constructor (deriveMatch val) CaseInsensitive val

mkStarts :: (MatchType -> CaseSensitivity -> Text -> Locator) -> Text -> Locator
mkStarts constructor val = constructor Starts CaseInsensitive val

mkExact :: (MatchType -> CaseSensitivity -> Text -> Locator) -> Text -> Locator
mkExact constructor val = constructor Full CaseSensitive val

elmClass :: Text -> Locator
elmClass = mkDefaults elmClass'

elmClass' :: MatchType -> CaseSensitivity -> Text -> Locator
elmClass' = mkPassThrough Class

elmClassExact :: Text -> Locator
elmClassExact = mkExact elmClass'

elemClassStarts :: Text -> Locator
elemClassStarts = mkStarts elmClass'

attribute :: Text -> Locator
attribute = mkDefaults attribute'

attribute' :: MatchType -> CaseSensitivity -> Text -> Locator
attribute' = mkPassThrough Attribute

attributeExact :: Text -> Locator
attributeExact = mkExact attribute'

attributeStarts :: Text -> Locator
attributeStarts = mkStarts attribute'

value :: Text -> Text -> Locator
value desc = mkDefaults $ value' desc

value' :: Text -> MatchType -> CaseSensitivity -> Text -> Locator
value' description matchType caseSensitivity value'' = PostFilter $ ValuePostFilter {description, matchType, caseSensitivity, value = value''}

valueExact :: Text -> Text -> Locator
valueExact description = mkExact (value' description)

valueStarts :: Text -> Text -> Locator
valueStarts description = mkStarts (value' description)

valueFunc :: Text -> (Text -> Bool) -> Locator
valueFunc description  = PostFilter . ValueFuncPostFilter description

------- Role Constructors -------

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

--- Tag Constructors ---

customTag :: Text -> Locator
customTag = Tag

a_ :: Locator
a_ = Tag "a"

abbr_ :: Locator
abbr_ = Tag "abbr"

address_ :: Locator
address_ = Tag "address"

area_ :: Locator
area_ = Tag "area"

article_ :: Locator
article_ = Tag "article"

aside_ :: Locator
aside_ = Tag "aside"

audio_ :: Locator
audio_ = Tag "audio"

b_ :: Locator
b_ = Tag "b"

base_ :: Locator
base_ = Tag "base"

bdi_ :: Locator
bdi_ = Tag "bdi"

bdo_ :: Locator
bdo_ = Tag "bdo"

blockquote_ :: Locator
blockquote_ = Tag "blockquote"

body_ :: Locator
body_ = Tag "body"

br_ :: Locator
br_ = Tag "br"

button_ :: Locator
button_ = Tag "button"

canvas_ :: Locator
canvas_ = Tag "canvas"

caption_ :: Locator
caption_ = Tag "caption"

cite_ :: Locator
cite_ = Tag "cite"

code_ :: Locator
code_ = Tag "code"

col_ :: Locator
col_ = Tag "col"

colgroup_ :: Locator
colgroup_ = Tag "colgroup"

data_ :: Locator
data_ = Tag "data"

datalist_ :: Locator
datalist_ = Tag "datalist"

dd_ :: Locator
dd_ = Tag "dd"

del_ :: Locator
del_ = Tag "del"

details_ :: Locator
details_ = Tag "details"

dfn_ :: Locator
dfn_ = Tag "dfn"

dialog_ :: Locator
dialog_ = Tag "dialog"

div_ :: Locator
div_ = Tag "div"

dl_ :: Locator
dl_ = Tag "dl"

dt_ :: Locator
dt_ = Tag "dt"

em_ :: Locator
em_ = Tag "em"

embed_ :: Locator
embed_ = Tag "embed"

fieldset_ :: Locator
fieldset_ = Tag "fieldset"

figcaption_ :: Locator
figcaption_ = Tag "figcaption"

figure_ :: Locator
figure_ = Tag "figure"

footer_ :: Locator
footer_ = Tag "footer"

form_ :: Locator
form_ = Tag "form"

h1_ :: Locator
h1_ = Tag "h1"

h2_ :: Locator
h2_ = Tag "h2"

h3_ :: Locator
h3_ = Tag "h3"

h4_ :: Locator
h4_ = Tag "h4"

h5_ :: Locator
h5_ = Tag "h5"

h6_ :: Locator
h6_ = Tag "h6"

head_ :: Locator
head_ = Tag "head"

header_ :: Locator
header_ = Tag "header"

hgroup_ :: Locator
hgroup_ = Tag "hgroup"

hr_ :: Locator
hr_ = Tag "hr"

html_ :: Locator
html_ = Tag "html"

i_ :: Locator
i_ = Tag "i"

iframe_ :: Locator
iframe_ = Tag "iframe"

img_ :: Locator
img_ = Tag "img"

input_ :: Locator
input_ = Tag "input"

ins_ :: Locator
ins_ = Tag "ins"

kbd_ :: Locator
kbd_ = Tag "kbd"

label_ :: Locator
label_ = Tag "label"

legend_ :: Locator
legend_ = Tag "legend"

li_ :: Locator
li_ = Tag "li"

link_ :: Locator
link_ = Tag "link"

main_ :: Locator
main_ = Tag "main"

map_ :: Locator
map_ = Tag "map"

mark_ :: Locator
mark_ = Tag "mark"

menu_ :: Locator
menu_ = Tag "menu"

meta_ :: Locator
meta_ = Tag "meta"

meter_ :: Locator
meter_ = Tag "meter"

nav_ :: Locator
nav_ = Tag "nav"

noscript_ :: Locator
noscript_ = Tag "noscript"

object_ :: Locator
object_ = Tag "object"

ol_ :: Locator
ol_ = Tag "ol"

optgroup_ :: Locator
optgroup_ = Tag "optgroup"

option_ :: Locator
option_ = Tag "option"

output_ :: Locator
output_ = Tag "output"

p_ :: Locator
p_ = Tag "p"

picture_ :: Locator
picture_ = Tag "picture"

pre_ :: Locator
pre_ = Tag "pre"

progress_ :: Locator
progress_ = Tag "progress"

q_ :: Locator
q_ = Tag "q"

rp_ :: Locator
rp_ = Tag "rp"

rt_ :: Locator
rt_ = Tag "rt"

ruby_ :: Locator
ruby_ = Tag "ruby"

s_ :: Locator
s_ = Tag "s"

samp_ :: Locator
samp_ = Tag "samp"

script_ :: Locator
script_ = Tag "script"

search_ :: Locator
search_ = Tag "search"

section_ :: Locator
section_ = Tag "section"

select_ :: Locator
select_ = Tag "select"

slot_ :: Locator
slot_ = Tag "slot"

small_ :: Locator
small_ = Tag "small"

source_ :: Locator
source_ = Tag "source"

span_ :: Locator
span_ = Tag "span"

strong_ :: Locator
strong_ = Tag "strong"

style_ :: Locator
style_ = Tag "style"

sub_ :: Locator
sub_ = Tag "sub"

summary_ :: Locator
summary_ = Tag "summary"

sup_ :: Locator
sup_ = Tag "sup"

table_ :: Locator
table_ = Tag "table"

tbody_ :: Locator
tbody_ = Tag "tbody"

td_ :: Locator
td_ = Tag "td"

template_ :: Locator
template_ = Tag "template"

textarea_ :: Locator
textarea_ = Tag "textarea"

tfoot_ :: Locator
tfoot_ = Tag "tfoot"

th_ :: Locator
th_ = Tag "th"

thead_ :: Locator
thead_ = Tag "thead"

time_ :: Locator
time_ = Tag "time"

title_ :: Locator
title_ = Tag "title"

tr_ :: Locator
tr_ = Tag "tr"

track_ :: Locator
track_ = Tag "track"

u_ :: Locator
u_ = Tag "u"

ul_ :: Locator
ul_ = Tag "ul"

var_ :: Locator
var_ = Tag "var"

video_ :: Locator
video_ = Tag "video"

wbr_ :: Locator
wbr_ = Tag "wbr"

--- Combinators ---

(&&&) :: Locator -> Locator -> Locator
l &&& r = All (l :| [r])

infixr 3 &&&

(|||) :: Locator -> Locator -> Locator
l ||| r = Any (l :| [r])

infixr 2 |||

(>>>) :: Locator -> Locator -> Locator
(>>>) = Parent

infixr 1 >>>

notLoc :: Locator -> Locator
notLoc l = None (l :| [])
