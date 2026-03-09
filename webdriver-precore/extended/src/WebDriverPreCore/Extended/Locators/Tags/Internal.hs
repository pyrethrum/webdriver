module WebDriverPreCore.Extended.Locators.Tags.Internal
  ( HTag,
    displayHTag,
    Tag,
    -- tag constructors
    mkCustomTag,
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
    -- utilities
    displayTag,
    tagToXPath,
  )
where

import Data.Text (Text, pack, toLower)
import Prelude

mkCustomTag :: Text -> Tag
mkCustomTag = Custom

t :: HTag -> Tag
t = Standard

a_ :: Tag
a_ = t A

abbr_ :: Tag
abbr_ = t Abbr

address_ :: Tag
address_ = t Address

area_ :: Tag
area_ = t Area

article_ :: Tag
article_ = t Article

aside_ :: Tag
aside_ = t Aside

audio_ :: Tag
audio_ = t Audio

b_ :: Tag
b_ = t B

base_ :: Tag
base_ = t Base

bdi_ :: Tag
bdi_ = t Bdi

bdo_ :: Tag
bdo_ = t Bdo

blockquote_ :: Tag
blockquote_ = t Blockquote

body_ :: Tag
body_ = t Body

br_ :: Tag
br_ = t Br

button_ :: Tag
button_ = t Button

canvas_ :: Tag
canvas_ = t Canvas

caption_ :: Tag
caption_ = t Caption

cite_ :: Tag
cite_ = t Cite

code_ :: Tag
code_ = t Code

col_ :: Tag
col_ = t Col

colgroup_ :: Tag
colgroup_ = t Colgroup

data_ :: Tag
data_ = t Data

datalist_ :: Tag
datalist_ = t Datalist

dd_ :: Tag
dd_ = t Dd

del_ :: Tag
del_ = t Del

details_ :: Tag
details_ = t Details

dfn_ :: Tag
dfn_ = t Dfn

dialog_ :: Tag
dialog_ = t Dialog

div_ :: Tag
div_ = t Div

dl_ :: Tag
dl_ = t Dl

dt_ :: Tag
dt_ = t Dt

em_ :: Tag
em_ = t Em

embed_ :: Tag
embed_ = t Embed

fieldset_ :: Tag
fieldset_ = t Fieldset

figcaption_ :: Tag
figcaption_ = t Figcaption

figure_ :: Tag
figure_ = t Figure

footer_ :: Tag
footer_ = t Footer

form_ :: Tag
form_ = t Form

h1_ :: Tag
h1_ = t H1

h2_ :: Tag
h2_ = t H2

h3_ :: Tag
h3_ = t H3

h4_ :: Tag
h4_ = t H4

h5_ :: Tag
h5_ = t H5

h6_ :: Tag
h6_ = t H6

head_ :: Tag
head_ = t Head

header_ :: Tag
header_ = t Header

hgroup_ :: Tag
hgroup_ = t Hgroup

hr_ :: Tag
hr_ = t Hr

html_ :: Tag
html_ = t Html

i_ :: Tag
i_ = t I

iframe_ :: Tag
iframe_ = t Iframe

img_ :: Tag
img_ = t Img

input_ :: Tag
input_ = t Input

ins_ :: Tag
ins_ = t Ins

kbd_ :: Tag
kbd_ = t Kbd

label_ :: Tag
label_ = t Label

legend_ :: Tag
legend_ = t Legend

li_ :: Tag
li_ = t Li

link_ :: Tag
link_ = t Link

main_ :: Tag
main_ = t Main

map_ :: Tag
map_ = t Map

mark_ :: Tag
mark_ = t Mark

menu_ :: Tag
menu_ = t Menu

meta_ :: Tag
meta_ = t Meta

meter_ :: Tag
meter_ = t Meter

nav_ :: Tag
nav_ = t Nav

noscript_ :: Tag
noscript_ = t Noscript

object_ :: Tag
object_ = t Object

ol_ :: Tag
ol_ = t Ol

optgroup_ :: Tag
optgroup_ = t Optgroup

option_ :: Tag
option_ = t Option

output_ :: Tag
output_ = t Output

p_ :: Tag
p_ = t P

picture_ :: Tag
picture_ = t Picture

pre_ :: Tag
pre_ = t Pre

progress_ :: Tag
progress_ = t Progress

q_ :: Tag
q_ = t Q

rp_ :: Tag
rp_ = t Rp

rt_ :: Tag
rt_ = t Rt

ruby_ :: Tag
ruby_ = t Ruby

s_ :: Tag
s_ = t S

samp_ :: Tag
samp_ = t Samp

script_ :: Tag
script_ = t Script

search_ :: Tag
search_ = t Search

section_ :: Tag
section_ = t Section

select_ :: Tag
select_ = t Select

slot_ :: Tag
slot_ = t Slot

small_ :: Tag
small_ = t Small

source_ :: Tag
source_ = t Source

span_ :: Tag
span_ = t Span

strong_ :: Tag
strong_ = t Strong

style_ :: Tag
style_ = t Style

sub_ :: Tag
sub_ = t Sub

summary_ :: Tag
summary_ = t Summary

sup_ :: Tag
sup_ = t Sup

table_ :: Tag
table_ = t Table

tbody_ :: Tag
tbody_ = t Tbody

td_ :: Tag
td_ = t Td

template_ :: Tag
template_ = t Template

textarea_ :: Tag
textarea_ = t Textarea

tfoot_ :: Tag
tfoot_ = t Tfoot

th_ :: Tag
th_ = t Th

thead_ :: Tag
thead_ = t Thead

time_ :: Tag
time_ = t Time

title_ :: Tag
title_ = t Title

tr_ :: Tag
tr_ = t Tr

track_ :: Tag
track_ = t Track

u_ :: Tag
u_ = t U

ul_ :: Tag
ul_ = t Ul

var_ :: Tag
var_ = t Var

video_ :: Tag
video_ = t Video

wbr_ :: Tag
wbr_ = t Wbr

-- | Standard HTML tag names.
--   Source: https://html.spec.whatwg.org/multipage/indices.html#elements-3
--   WHATWG HTML Living Standard, retrieved 2026-03-03
data HTag
  = A
  | Abbr
  | Address
  | Area
  | Article
  | Aside
  | Audio
  | B
  | Base
  | Bdi
  | Bdo
  | Blockquote
  | Body
  | Br
  | Button
  | Canvas
  | Caption
  | Cite
  | Code
  | Col
  | Colgroup
  | Data
  | Datalist
  | Dd
  | Del
  | Details
  | Dfn
  | Dialog
  | Div
  | Dl
  | Dt
  | Em
  | Embed
  | Fieldset
  | Figcaption
  | Figure
  | Footer
  | Form
  | H1
  | H2
  | H3
  | H4
  | H5
  | H6
  | Head
  | Header
  | Hgroup
  | Hr
  | Html
  | I
  | Iframe
  | Img
  | Input
  | Ins
  | Kbd
  | Label
  | Legend
  | Li
  | Link
  | Main
  | Map
  | Mark
  | Menu
  | Meta
  | Meter
  | Nav
  | Noscript
  | Object
  | Ol
  | Optgroup
  | Option
  | Output
  | P
  | Picture
  | Pre
  | Progress
  | Q
  | Rp
  | Rt
  | Ruby
  | S
  | Samp
  | Script
  | Search
  | Section
  | Select
  | Slot
  | Small
  | Source
  | Span
  | Strong
  | Style
  | Sub
  | Summary
  | Sup
  | Table
  | Tbody
  | Td
  | Template
  | Textarea
  | Tfoot
  | Th
  | Thead
  | Time
  | Title
  | Tr
  | Track
  | U
  | Ul
  | Var
  | Video
  | Wbr
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Display an HTag as its lowercase HTML tag name.
displayHTag :: HTag -> Text
displayHTag = toLower . pack . show

-- | Either a standard HTML tag or a custom element tag
data Tag
  = Standard HTag
  | Custom Text
  deriving (Show, Eq)

-- | Display a Tag as its lowercase tag name string.
displayTag :: Tag -> Text
displayTag (Standard htag) = displayHTag htag
displayTag (Custom txt) = txt

-- | XPath expression matching all elements with the given tag name.
tagToXPath :: Tag -> Text
tagToXPath = ("//" <>) . displayTag
