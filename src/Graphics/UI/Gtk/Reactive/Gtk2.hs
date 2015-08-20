module Graphics.UI.Gtk.Reactive.Gtk2 where

import           Control.Monad (void)
import           Data.ByteString
import           Data.ReactiveValue
import           Data.Word
import qualified Foreign.C.Types as C
import           Graphics.UI.Gtk
import           Graphics.UI.Gtk.Reactive.Property
import           Graphics.Rendering.Cairo.Types (Cairo, FontOptions, Matrix)
import           System.Glib

-- @G: cairoContextGetFontOptions					 | ["cairo","Context","Get","Font","Options"]
cairoContextGetFontOptionsPassive :: (PangoContext) -> ReactiveFieldRead IO (FontOptions)
cairoContextGetFontOptionsPassive w = wrapMRPassive (cairoContextGetFontOptions w)


-- @G: cairoContextGetResolution					 | ["cairo","Context","Get","Resolution"]
cairoContextGetResolutionPassive :: (PangoContext) -> ReactiveFieldRead IO (Double)
cairoContextGetResolutionPassive w = wrapMRPassive (cairoContextGetResolution w)


-- @T: cairoContextSetFontOptions					 | ["cairo","Context","Set","Font","Options"]
cairoContextSetFontOptionsPassive :: (PangoContext) -> ReactiveFieldWrite IO (FontOptions)
cairoContextSetFontOptionsPassive w = wrapMW (cairoContextSetFontOptions w)

-- @T: cairoContextSetResolution					 | ["cairo","Context","Set","Resolution"]
cairoContextSetResolutionPassive :: (PangoContext) -> ReactiveFieldWrite IO (Double)
cairoContextSetResolutionPassive w = wrapMW (cairoContextSetResolution w)

-- @G: cairoFontMapGetDefault					 | ["cairo","Font","Map","Get","Default"]
cairoFontMapGetDefaultPassive :: ReactiveFieldRead IO (FontMap)
cairoFontMapGetDefaultPassive = wrapMRPassive (cairoFontMapGetDefault)


-- @G: cairoFontMapGetResolution					 | ["cairo","Font","Map","Get","Resolution"]
cairoFontMapGetResolutionPassive :: (FontMap) -> ReactiveFieldRead IO (Double)
cairoFontMapGetResolutionPassive w = wrapMRPassive (cairoFontMapGetResolution w)


-- @T: cairoFontMapSetResolution					 | ["cairo","Font","Map","Set","Resolution"]
cairoFontMapSetResolutionPassive :: (FontMap) -> ReactiveFieldWrite IO (Double)
cairoFontMapSetResolutionPassive w = wrapMW (cairoFontMapSetResolution w)

-- @T: setSourceColor					 | ["set","Source","Color"]
-- TODO
-- @G: contextGetFontDescription					 | ["context","Get","Font","Description"]
contextGetFontDescriptionPassive :: (PangoContext) -> ReactiveFieldRead IO (FontDescription)
contextGetFontDescriptionPassive w = wrapMRPassive (contextGetFontDescription w)


-- @G: contextGetLanguage					 | ["context","Get","Language"]
contextGetLanguagePassive :: (PangoContext) -> ReactiveFieldRead IO (Language)
contextGetLanguagePassive w = wrapMRPassive (contextGetLanguage w)


-- @G: contextGetMatrix					 | ["context","Get","Matrix"]
contextGetMatrixPassive :: (PangoContext) -> ReactiveFieldRead IO (Matrix)
contextGetMatrixPassive w = wrapMRPassive (contextGetMatrix w)


-- @G: contextGetMetrics					 | ["context","Get","Metrics"]
-- TODO
-- @G: contextGetTextDir					 | ["context","Get","Text","Dir"]
contextGetTextDirPassive :: (PangoContext) -> ReactiveFieldRead IO (PangoDirection)
contextGetTextDirPassive w = wrapMRPassive (contextGetTextDir w)


-- @G: contextGetTextGravity					 | ["context","Get","Text","Gravity"]
contextGetTextGravityPassive :: (PangoContext) -> ReactiveFieldRead IO (PangoGravity)
contextGetTextGravityPassive w = wrapMRPassive (contextGetTextGravity w)


-- @G: contextGetTextGravityHint					 | ["context","Get","Text","Gravity","Hint"]
contextGetTextGravityHintPassive :: (PangoContext) -> ReactiveFieldRead IO (PangoGravityHint)
contextGetTextGravityHintPassive w = wrapMRPassive (contextGetTextGravityHint w)


-- @T: contextSetFontDescription					 | ["context","Set","Font","Description"]
contextSetFontDescriptionPassive :: (PangoContext) -> ReactiveFieldWrite IO (FontDescription)
contextSetFontDescriptionPassive w = wrapMW (contextSetFontDescription w)

-- @T: contextSetLanguage					 | ["context","Set","Language"]
contextSetLanguagePassive :: (PangoContext) -> ReactiveFieldWrite IO (Language)
contextSetLanguagePassive w = wrapMW (contextSetLanguage w)

-- @T: contextSetMatrix					 | ["context","Set","Matrix"]
contextSetMatrixPassive :: (PangoContext) -> ReactiveFieldWrite IO (Matrix)
contextSetMatrixPassive w = wrapMW (contextSetMatrix w)

-- @T: contextSetTextDir					 | ["context","Set","Text","Dir"]
contextSetTextDirPassive :: (PangoContext) -> ReactiveFieldWrite IO (PangoDirection)
contextSetTextDirPassive w = wrapMW (contextSetTextDir w)

-- @T: contextSetTextGravity					 | ["context","Set","Text","Gravity"]
contextSetTextGravityPassive :: (PangoContext) -> ReactiveFieldWrite IO (PangoGravity)
contextSetTextGravityPassive w = wrapMW (contextSetTextGravity w)

-- @T: contextSetTextGravityHint					 | ["context","Set","Text","Gravity","Hint"]
contextSetTextGravityHintPassive :: (PangoContext) -> ReactiveFieldWrite IO (PangoGravityHint)
contextSetTextGravityHintPassive w = wrapMW (contextSetTextGravityHint w)

-- @G: layoutGetAlignment					 | ["layout","Get","Alignment"]
layoutGetAlignmentPassive :: (PangoLayout) -> ReactiveFieldRead IO (LayoutAlignment)
layoutGetAlignmentPassive w = wrapMRPassive (layoutGetAlignment w)


-- @G: layoutGetAttributes					 | ["layout","Get","Attributes"]
layoutGetAttributesPassive :: (PangoLayout) -> ReactiveFieldRead IO ([[PangoAttribute]])
layoutGetAttributesPassive w = wrapMRPassive (layoutGetAttributes w)


-- @G: layoutGetAutoDir					 | ["layout","Get","Auto","Dir"]
layoutGetAutoDirPassive :: (PangoLayout) -> ReactiveFieldRead IO (Bool)
layoutGetAutoDirPassive w = wrapMRPassive (layoutGetAutoDir w)


-- @G: layoutGetContext					 | ["layout","Get","Context"]
layoutGetContextPassive :: (PangoLayout) -> ReactiveFieldRead IO (PangoContext)
layoutGetContextPassive w = wrapMRPassive (layoutGetContext w)


-- @G: layoutGetCursorPos					 | ["layout","Get","Cursor","Pos"]
-- TODO
-- @G: layoutGetEllipsize					 | ["layout","Get","Ellipsize"]
layoutGetEllipsizePassive :: (PangoLayout) -> ReactiveFieldRead IO (EllipsizeMode)
layoutGetEllipsizePassive w = wrapMRPassive (layoutGetEllipsize w)


-- @G: layoutGetExtents					 | ["layout","Get","Extents"]
layoutGetExtentsPassive :: (PangoLayout) -> ReactiveFieldRead IO ((PangoRectangle, PangoRectangle))
layoutGetExtentsPassive w = wrapMRPassive (layoutGetExtents w)


-- @G: layoutGetFontDescription					 | ["layout","Get","Font","Description"]
layoutGetFontDescriptionPassive :: (PangoLayout) -> ReactiveFieldRead IO ((Maybe FontDescription))
layoutGetFontDescriptionPassive w = wrapMRPassive (layoutGetFontDescription w)


-- @G: layoutGetIndent					 | ["layout","Get","Indent"]
layoutGetIndentPassive :: (PangoLayout) -> ReactiveFieldRead IO (Double)
layoutGetIndentPassive w = wrapMRPassive (layoutGetIndent w)


-- @G: layoutGetIter					 | ["layout","Get","Iter"]
layoutGetIterPassive :: (PangoLayout) -> ReactiveFieldRead IO (LayoutIter)
layoutGetIterPassive w = wrapMRPassive (layoutGetIter w)


-- @G: layoutGetJustify					 | ["layout","Get","Justify"]
layoutGetJustifyPassive :: (PangoLayout) -> ReactiveFieldRead IO (Bool)
layoutGetJustifyPassive w = wrapMRPassive (layoutGetJustify w)


-- @G: layoutGetLineCount					 | ["layout","Get","Line","Count"]
layoutGetLineCountPassive :: (PangoLayout) -> ReactiveFieldRead IO (Int)
layoutGetLineCountPassive w = wrapMRPassive (layoutGetLineCount w)


-- @G: layoutGetLine					 | ["layout","Get","Line"]
-- TODO
-- @G: layoutGetLines					 | ["layout","Get","Lines"]
layoutGetLinesPassive :: (PangoLayout) -> ReactiveFieldRead IO ([LayoutLine])
layoutGetLinesPassive w = wrapMRPassive (layoutGetLines w)


-- @G: layoutGetPixelExtents					 | ["layout","Get","Pixel","Extents"]
layoutGetPixelExtentsPassive :: (PangoLayout) -> ReactiveFieldRead IO ((Rectangle, Rectangle))
layoutGetPixelExtentsPassive w = wrapMRPassive (layoutGetPixelExtents w)


-- @G: layoutGetSingleParagraphMode					 | ["layout","Get","Single","Paragraph","Mode"]
layoutGetSingleParagraphModePassive :: (PangoLayout) -> ReactiveFieldRead IO (Bool)
layoutGetSingleParagraphModePassive w = wrapMRPassive (layoutGetSingleParagraphMode w)


-- @G: layoutGetSpacing					 | ["layout","Get","Spacing"]
layoutGetSpacingPassive :: (PangoLayout) -> ReactiveFieldRead IO (Double)
layoutGetSpacingPassive w = wrapMRPassive (layoutGetSpacing w)


-- @G: layoutGetTabs					 | ["layout","Get","Tabs"]
layoutGetTabsPassive :: (PangoLayout) -> ReactiveFieldRead IO ((Maybe [TabPosition]))
layoutGetTabsPassive w = wrapMRPassive (layoutGetTabs w)


-- @G: layoutGetText					 | ["layout","Get","Text"]
layoutGetTextPassive :: GlibString string => (PangoLayout) -> ReactiveFieldRead IO (string)
layoutGetTextPassive w = wrapMRPassive (layoutGetText w)


-- @G: layoutGetWidth					 | ["layout","Get","Width"]
layoutGetWidthPassive :: (PangoLayout) -> ReactiveFieldRead IO ((Maybe Double))
layoutGetWidthPassive w = wrapMRPassive (layoutGetWidth w)


-- @G: layoutGetWrap					 | ["layout","Get","Wrap"]
layoutGetWrapPassive :: (PangoLayout) -> ReactiveFieldRead IO (LayoutWrapMode)
layoutGetWrapPassive w = wrapMRPassive (layoutGetWrap w)


-- @G: layoutIterGetBaseline					 | ["layout","Iter","Get","Baseline"]
layoutIterGetBaselinePassive :: (LayoutIter) -> ReactiveFieldRead IO (Double)
layoutIterGetBaselinePassive w = wrapMRPassive (layoutIterGetBaseline w)


-- @G: layoutIterGetCharExtents					 | ["layout","Iter","Get","Char","Extents"]
layoutIterGetCharExtentsPassive :: (LayoutIter) -> ReactiveFieldRead IO (PangoRectangle)
layoutIterGetCharExtentsPassive w = wrapMRPassive (layoutIterGetCharExtents w)


-- @G: layoutIterGetClusterExtents					 | ["layout","Iter","Get","Cluster","Extents"]
layoutIterGetClusterExtentsPassive :: (LayoutIter) -> ReactiveFieldRead IO ((PangoRectangle, PangoRectangle))
layoutIterGetClusterExtentsPassive w = wrapMRPassive (layoutIterGetClusterExtents w)


-- @G: layoutIterGetIndex					 | ["layout","Iter","Get","Index"]
layoutIterGetIndexPassive :: (LayoutIter) -> ReactiveFieldRead IO (Int)
layoutIterGetIndexPassive w = wrapMRPassive (layoutIterGetIndex w)


-- @G: layoutIterGetItem					 | ["layout","Iter","Get","Item"]
layoutIterGetItemPassive :: (LayoutIter) -> ReactiveFieldRead IO ((Maybe GlyphItem))
layoutIterGetItemPassive w = wrapMRPassive (layoutIterGetItem w)


-- @G: layoutIterGetLineExtents					 | ["layout","Iter","Get","Line","Extents"]
layoutIterGetLineExtentsPassive :: (LayoutIter) -> ReactiveFieldRead IO ((PangoRectangle, PangoRectangle))
layoutIterGetLineExtentsPassive w = wrapMRPassive (layoutIterGetLineExtents w)


-- @G: layoutIterGetLine					 | ["layout","Iter","Get","Line"]
layoutIterGetLinePassive :: (LayoutIter) -> ReactiveFieldRead IO ((Maybe LayoutLine))
layoutIterGetLinePassive w = wrapMRPassive (layoutIterGetLine w)


-- @G: layoutIterGetLineYRange					 | ["layout","Iter","Get","Line","YRange"]
layoutIterGetLineYRangePassive :: (LayoutIter) -> ReactiveFieldRead IO ((Double, Double))
layoutIterGetLineYRangePassive w = wrapMRPassive (layoutIterGetLineYRange w)


-- @G: layoutIterGetRunExtents					 | ["layout","Iter","Get","Run","Extents"]
layoutIterGetRunExtentsPassive :: (LayoutIter) -> ReactiveFieldRead IO ((PangoRectangle, PangoRectangle))
layoutIterGetRunExtentsPassive w = wrapMRPassive (layoutIterGetRunExtents w)


-- @G: layoutLineGetExtents					 | ["layout","Line","Get","Extents"]
layoutLineGetExtentsPassive :: (LayoutLine) -> ReactiveFieldRead IO ((PangoRectangle, PangoRectangle))
layoutLineGetExtentsPassive w = wrapMRPassive (layoutLineGetExtents w)


-- @G: layoutLineGetPixelExtents					 | ["layout","Line","Get","Pixel","Extents"]
layoutLineGetPixelExtentsPassive :: (LayoutLine) -> ReactiveFieldRead IO ((Rectangle, Rectangle))
layoutLineGetPixelExtentsPassive w = wrapMRPassive (layoutLineGetPixelExtents w)


-- @G: layoutLineGetXRanges					 | ["layout","Line","Get","XRanges"]
-- TODO
-- @T: layoutSetAlignment					 | ["layout","Set","Alignment"]
layoutSetAlignmentPassive :: (PangoLayout) -> ReactiveFieldWrite IO (LayoutAlignment)
layoutSetAlignmentPassive w = wrapMW (layoutSetAlignment w)

-- @T: layoutSetAttributes					 | ["layout","Set","Attributes"]
layoutSetAttributesPassive :: (PangoLayout) -> ReactiveFieldWrite IO ([PangoAttribute])
layoutSetAttributesPassive w = wrapMW (layoutSetAttributes w)

-- @T: layoutSetAutoDir					 | ["layout","Set","Auto","Dir"]
layoutSetAutoDirPassive :: (PangoLayout) -> ReactiveFieldWrite IO (Bool)
layoutSetAutoDirPassive w = wrapMW (layoutSetAutoDir w)

-- @T: layoutSetEllipsize					 | ["layout","Set","Ellipsize"]
layoutSetEllipsizePassive :: (PangoLayout) -> ReactiveFieldWrite IO (EllipsizeMode)
layoutSetEllipsizePassive w = wrapMW (layoutSetEllipsize w)

-- @T: layoutSetFontDescription					 | ["layout","Set","Font","Description"]
layoutSetFontDescriptionPassive :: (PangoLayout) -> ReactiveFieldWrite IO (Maybe FontDescription)
layoutSetFontDescriptionPassive w = wrapMW (layoutSetFontDescription w)

-- @T: layoutSetIndent					 | ["layout","Set","Indent"]
layoutSetIndentPassive :: (PangoLayout) -> ReactiveFieldWrite IO (Double)
layoutSetIndentPassive w = wrapMW (layoutSetIndent w)

-- @T: layoutSetJustify					 | ["layout","Set","Justify"]
layoutSetJustifyPassive :: (PangoLayout) -> ReactiveFieldWrite IO (Bool)
layoutSetJustifyPassive w = wrapMW (layoutSetJustify w)

-- @T: layoutSetSingleParagraphMode					 | ["layout","Set","Single","Paragraph","Mode"]
layoutSetSingleParagraphModePassive :: (PangoLayout) -> ReactiveFieldWrite IO (Bool)
layoutSetSingleParagraphModePassive w = wrapMW (layoutSetSingleParagraphMode w)

-- @T: layoutSetSpacing					 | ["layout","Set","Spacing"]
layoutSetSpacingPassive :: (PangoLayout) -> ReactiveFieldWrite IO (Double)
layoutSetSpacingPassive w = wrapMW (layoutSetSpacing w)

-- @T: layoutSetTabs					 | ["layout","Set","Tabs"]
layoutSetTabsPassive :: (PangoLayout) -> ReactiveFieldWrite IO ([TabPosition])
layoutSetTabsPassive w = wrapMW (layoutSetTabs w)

-- @T: layoutSetText					 | ["layout","Set","Text"]
layoutSetTextReactive :: GlibString string => (PangoLayout) -> ReactiveFieldWrite IO (string)
layoutSetTextReactive w = wrapMW (layoutSetText w)


-- @T: layoutSetWidth					 | ["layout","Set","Width"]
layoutSetWidthPassive :: (PangoLayout) -> ReactiveFieldWrite IO (Maybe Double)
layoutSetWidthPassive w = wrapMW (layoutSetWidth w)

-- @T: layoutSetWrap					 | ["layout","Set","Wrap"]
layoutSetWrapPassive :: (PangoLayout) -> ReactiveFieldWrite IO (LayoutWrapMode)
layoutSetWrapPassive w = wrapMW (layoutSetWrap w)

-- @G: pangoItemGetFontMetrics					 | ["pango","Item","Get","Font","Metrics"]
pangoItemGetFontMetricsPassive :: (PangoItem) -> ReactiveFieldRead IO (FontMetrics)
pangoItemGetFontMetricsPassive w = wrapMRPassive (pangoItemGetFontMetrics w)


-- @G: pangoItemGetFont					 | ["pango","Item","Get","Font"]
pangoItemGetFontPassive :: (PangoItem) -> ReactiveFieldRead IO (Font)
pangoItemGetFontPassive w = wrapMRPassive (pangoItemGetFont w)


-- @G: pangoItemGetLanguage					 | ["pango","Item","Get","Language"]
pangoItemGetLanguagePassive :: (PangoItem) -> ReactiveFieldRead IO (Language)
pangoItemGetLanguagePassive w = wrapMRPassive (pangoItemGetLanguage w)


-- @G: binGetChild					 | ["bin","Get","Child"]
binGetChildPassive :: BinClass self => (self) -> ReactiveFieldRead IO ((Maybe Widget))
binGetChildPassive w = wrapMRPassive (binGetChild w)


-- @A: boxChildPacking
-- TODO
-- @A: boxChildPackType
-- TODO
-- @A: boxChildPadding
-- TODO
-- @A: boxChildPosition
-- TODO
-- @G: boxGetHomogeneous					 | ["box","Get","Homogeneous"]
boxGetHomogeneousPassive :: BoxClass self => (self) -> ReactiveFieldRead IO (Bool)
boxGetHomogeneousPassive w = wrapMRPassive (boxGetHomogeneous w)


-- @G: boxGetSpacing					 | ["box","Get","Spacing"]
boxGetSpacingPassive :: BoxClass self => (self) -> ReactiveFieldRead IO (Int)
boxGetSpacingPassive w = wrapMRPassive (boxGetSpacing w)


-- @A: boxHomogeneous
boxHomogeneousPassive :: BoxClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
boxHomogeneousPassive w = passivePropertyNE w boxHomogeneous


-- @T: boxSetChildPacking					 | ["box","Set","Child","Packing"]
-- TODO
-- @T: boxSetHomogeneous					 | ["box","Set","Homogeneous"]
boxSetHomogeneousReactive :: BoxClass self => (self) -> ReactiveFieldWrite IO (Bool)
boxSetHomogeneousReactive w = wrapMW (boxSetHomogeneous w)


-- @T: boxSetSpacing					 | ["box","Set","Spacing"]
boxSetSpacingReactive :: BoxClass self => (self) -> ReactiveFieldWrite IO (Int)
boxSetSpacingReactive w = wrapMW (boxSetSpacing w)


-- @A: boxSpacing
boxSpacingPassive :: BoxClass self => (self) -> ReactiveFieldReadWrite IO (Int)
boxSpacingPassive w = passivePropertyNE w boxSpacing


-- @A: buttonBoxChildSecondary
-- TODO
-- @G: buttonBoxGetChildSecondary					 | ["button","Box","Get","Child","Secondary"]
-- TODO
-- @G: buttonBoxGetLayout					 | ["button","Box","Get","Layout"]
buttonBoxGetLayoutPassive :: ButtonBoxClass self => (self) -> ReactiveFieldRead IO (ButtonBoxStyle)
buttonBoxGetLayoutPassive w = wrapMRPassive (buttonBoxGetLayout w)


-- @A: buttonBoxLayoutStyle
buttonBoxLayoutStylePassive :: ButtonBoxClass self => (self) -> ReactiveFieldReadWrite IO (ButtonBoxStyle)
buttonBoxLayoutStylePassive w = passivePropertyNE w buttonBoxLayoutStyle


-- @T: buttonBoxSetChildSecondary					 | ["button","Box","Set","Child","Secondary"]
-- TODO
-- @T: buttonBoxSetLayout					 | ["button","Box","Set","Layout"]
buttonBoxSetLayoutReactive :: ButtonBoxClass self => (self) -> ReactiveFieldWrite IO (ButtonBoxStyle)
buttonBoxSetLayoutReactive w = wrapMW (buttonBoxSetLayout w)


-- @S: add
-- TODO
-- @C: afterAdd
-- TODO
-- @C: afterCheckResize
afterCheckResizeReactive :: ContainerClass self => self -> ReactiveFieldRead IO ()
afterCheckResizeReactive w = reactivePropertyH_ w afterCheckResize

-- @C: afterRemove
-- TODO
-- @C: afterSetFocusChild
-- TODO
-- @S: checkResize
checkResizeReactive :: ContainerClass self => self -> ReactiveFieldRead IO ()
checkResizeReactive = (`reactiveSignalIO` checkResize)


-- @A: containerBorderWidth
containerBorderWidthPassive :: ContainerClass self => (self) -> ReactiveFieldReadWrite IO (Int)
containerBorderWidthPassive w = passivePropertyNE w containerBorderWidth


-- @G: containerGetBorderWidth					 | ["container","Get","Border","Width"]
containerGetBorderWidthPassive :: ContainerClass self => (self) -> ReactiveFieldRead IO (Int)
containerGetBorderWidthPassive w = wrapMRPassive (containerGetBorderWidth w)


-- @G: containerGetChildren					 | ["container","Get","Children"]
containerGetChildrenPassive :: ContainerClass self => (self) -> ReactiveFieldRead IO ([Widget])
containerGetChildrenPassive w = wrapMRPassive (containerGetChildren w)


-- @G: containerGetFocusChain					 | ["container","Get","Focus","Chain"]
containerGetFocusChainPassive :: ContainerClass self => (self) -> ReactiveFieldRead IO ((Maybe [Widget]))
containerGetFocusChainPassive w = wrapMRPassive (containerGetFocusChain w)


-- @G: containerGetFocusHAdjustment					 | ["container","Get","Focus","HAdjustment"]
containerGetFocusHAdjustmentPassive :: ContainerClass self => (self) -> ReactiveFieldRead IO ((Maybe Adjustment))
containerGetFocusHAdjustmentPassive w = wrapMRPassive (containerGetFocusHAdjustment w)


-- @G: containerGetFocusVAdjustment					 | ["container","Get","Focus","VAdjustment"]
containerGetFocusVAdjustmentPassive :: ContainerClass self => (self) -> ReactiveFieldRead IO ((Maybe Adjustment))
containerGetFocusVAdjustmentPassive w = wrapMRPassive (containerGetFocusVAdjustment w)


-- @G: containerGetResizeMode					 | ["container","Get","Resize","Mode"]
containerGetResizeModePassive :: ContainerClass self => (self) -> ReactiveFieldRead IO (ResizeMode)
containerGetResizeModePassive w = wrapMRPassive (containerGetResizeMode w)


-- @A: containerResizeMode
containerResizeModePassive :: ContainerClass self => (self) -> ReactiveFieldReadWrite IO (ResizeMode)
containerResizeModePassive w = passivePropertyNE w containerResizeMode


-- @T: containerSetBorderWidth					 | ["container","Set","Border","Width"]
containerSetBorderWidthReactive :: ContainerClass self => (self) -> ReactiveFieldWrite IO (Int)
containerSetBorderWidthReactive w = wrapMW (containerSetBorderWidth w)


-- @T: containerSetFocusChain					 | ["container","Set","Focus","Chain"]
containerSetFocusChainReactive :: ContainerClass self => (self) -> ReactiveFieldWrite IO ([Widget])
containerSetFocusChainReactive w = wrapMW (containerSetFocusChain w)


-- @T: containerSetFocusChild					 | ["container","Set","Focus","Child"]
containerSetFocusChildReactive :: (ContainerClass self, WidgetClass child) => (self) -> ReactiveFieldWrite IO (child)
containerSetFocusChildReactive w = wrapMW (containerSetFocusChild w)


-- @T: containerSetFocusHAdjustment					 | ["container","Set","Focus","HAdjustment"]
containerSetFocusHAdjustmentReactive :: ContainerClass self => (self) -> ReactiveFieldWrite IO (Adjustment)
containerSetFocusHAdjustmentReactive w = wrapMW (containerSetFocusHAdjustment w)


-- @T: containerSetFocusVAdjustment					 | ["container","Set","Focus","VAdjustment"]
containerSetFocusVAdjustmentReactive :: ContainerClass self => (self) -> ReactiveFieldWrite IO (Adjustment)
containerSetFocusVAdjustmentReactive w = wrapMW (containerSetFocusVAdjustment w)


-- @T: containerSetResizeMode					 | ["container","Set","Resize","Mode"]
containerSetResizeModeReactive :: ContainerClass self => (self) -> ReactiveFieldWrite IO (ResizeMode)
containerSetResizeModeReactive w = wrapMW (containerSetResizeMode w)


-- @C: onAdd
-- TODO
-- @C: onCheckResize
onCheckResizeReactive :: ContainerClass self => self -> ReactiveFieldRead IO ()
onCheckResizeReactive w = reactivePropertyH_ w onCheckResize

-- @C: onRemove
-- TODO
-- @C: onSetFocusChild
-- TODO
-- @S: remove
-- TODO
-- @S: setFocusChild
-- TODO
-- @S: imContextCommit
-- TODO
-- @S: imContextDeleteSurrounding'
-- TODO
-- @G: imContextGetPreeditString					 | ["im","Context","Get","Preedit","String"]
imContextGetPreeditStringPassive :: (IMContextClass self, GlibString string) => (self) -> ReactiveFieldRead IO ((string, [[PangoAttribute]], Int))
imContextGetPreeditStringPassive w = wrapMRPassive (imContextGetPreeditString w)


-- @G: imContextGetSurrounding					 | ["im","Context","Get","Surrounding"]
imContextGetSurroundingPassive :: (IMContextClass self, GlibString string) => (self) -> ReactiveFieldRead IO ((Maybe (string, Int)))
imContextGetSurroundingPassive w = wrapMRPassive (imContextGetSurrounding w)


-- @S: imContextPreeditChanged
imContextPreeditChangedReactive :: IMContextClass self => self -> ReactiveFieldRead IO ()
imContextPreeditChangedReactive = (`reactiveSignalIO` imContextPreeditChanged)


-- @S: imContextPreeditEnd
imContextPreeditEndReactive :: IMContextClass self => self -> ReactiveFieldRead IO ()
imContextPreeditEndReactive = (`reactiveSignalIO` imContextPreeditEnd)


-- @S: imContextPreeditStart
imContextPreeditStartReactive :: IMContextClass self => self -> ReactiveFieldRead IO ()
imContextPreeditStartReactive = (`reactiveSignalIO` imContextPreeditStart)


-- @S: imContextRetrieveSurrounding
-- TODO
-- @T: imContextSetClientWindow					 | ["im","Context","Set","Client","Window"]
imContextSetClientWindowReactive :: IMContextClass self => (self) -> ReactiveFieldWrite IO (Maybe DrawWindow)
imContextSetClientWindowReactive w = wrapMW (imContextSetClientWindow w)


-- @T: imContextSetCursorLocation					 | ["im","Context","Set","Cursor","Location"]
imContextSetCursorLocationReactive :: IMContextClass self => (self) -> ReactiveFieldWrite IO (Rectangle)
imContextSetCursorLocationReactive w = wrapMW (imContextSetCursorLocation w)


-- @T: imContextSetSurrounding					 | ["im","Context","Set","Surrounding"]
-- TODO
-- @T: imContextSetUsePreedit					 | ["im","Context","Set","Use","Preedit"]
imContextSetUsePreeditReactive :: IMContextClass self => (self) -> ReactiveFieldWrite IO (Bool)
imContextSetUsePreeditReactive w = wrapMW (imContextSetUsePreedit w)


-- @G: miscGetAlignment					 | ["misc","Get","Alignment"]
miscGetAlignmentPassive :: MiscClass self => (self) -> ReactiveFieldRead IO ((Double, Double))
miscGetAlignmentPassive w = wrapMRPassive (miscGetAlignment w)


-- @G: miscGetPadding					 | ["misc","Get","Padding"]
miscGetPaddingPassive :: MiscClass self => (self) -> ReactiveFieldRead IO ((Int, Int))
miscGetPaddingPassive w = wrapMRPassive (miscGetPadding w)


-- @T: miscSetAlignment					 | ["misc","Set","Alignment"]
-- TODO
-- @T: miscSetPadding					 | ["misc","Set","Padding"]
-- TODO
-- @A: miscXalign
miscXalignPassive :: MiscClass self => (self) -> ReactiveFieldReadWrite IO (Float)
miscXalignPassive w = passivePropertyNE w miscXalign


-- @A: miscXpad
miscXpadPassive :: MiscClass self => (self) -> ReactiveFieldReadWrite IO (Int)
miscXpadPassive w = passivePropertyNE w miscXpad


-- @A: miscYalign
miscYalignPassive :: MiscClass self => (self) -> ReactiveFieldReadWrite IO (Float)
miscYalignPassive w = passivePropertyNE w miscYalign


-- @A: miscYpad
miscYpadPassive :: MiscClass self => (self) -> ReactiveFieldReadWrite IO (Int)
miscYpadPassive w = passivePropertyNE w miscYpad


-- @S: notifyProperty
-- TODO
-- @S: objectDestroy
objectDestroyReactive :: ObjectClass self => self -> ReactiveFieldRead IO ()
objectDestroyReactive = (`reactiveSignalIO` objectDestroy)


-- @C: afterAcceptPosition
afterAcceptPositionReactive :: PanedClass self => self -> ReactiveFieldRead IO ()
afterAcceptPositionReactive w = reactivePropertyH_ w (\x i -> afterAcceptPosition x (i >> return False))

-- @C: afterCancelPosition
afterCancelPositionReactive :: PanedClass self => self -> ReactiveFieldRead IO ()
afterCancelPositionReactive w = reactivePropertyH_ w (\x i -> afterCancelPosition x (i >> return False))

-- @C: afterCycleChildFocus
-- TODO
-- @C: afterCycleHandleFocus
-- TODO
-- @C: afterMoveHandle
-- TODO
-- @C: afterToggleHandleFocus
afterToggleHandleFocusReactive :: PanedClass self => self -> ReactiveFieldRead IO ()
afterToggleHandleFocusReactive w = reactivePropertyH_ w (\x i -> afterToggleHandleFocus x (i >> return False))

-- @C: onAcceptPosition
onAcceptPositionReactive :: PanedClass self => self -> ReactiveFieldRead IO ()
onAcceptPositionReactive w = reactivePropertyH_ w (\x i -> onAcceptPosition x (i >> return False))

-- @C: onCancelPosition
onCancelPositionReactive :: PanedClass self => self -> ReactiveFieldRead IO ()
onCancelPositionReactive w = reactivePropertyH_ w (\x i -> onCancelPosition x (i >> return False))

-- @C: onCycleChildFocus
-- TODO
-- @C: onCycleHandleFocus
-- TODO
-- @C: onMoveHandle
-- TODO
-- @C: onToggleHandleFocus
onToggleHandleFocusReactive :: PanedClass self => self -> ReactiveFieldRead IO ()
onToggleHandleFocusReactive w = reactivePropertyH_ w (\x i -> onToggleHandleFocus x (i >> return False))

-- @A: panedChildResize
-- TODO
-- @A: panedChildShrink
-- TODO
-- @G: panedGetChild1					 | ["paned","Get","Child1"]
panedGetChild1Passive :: PanedClass self => (self) -> ReactiveFieldRead IO ((Maybe Widget))
panedGetChild1Passive w = wrapMRPassive (panedGetChild1 w)


-- @G: panedGetChild2					 | ["paned","Get","Child2"]
panedGetChild2Passive :: PanedClass self => (self) -> ReactiveFieldRead IO ((Maybe Widget))
panedGetChild2Passive w = wrapMRPassive (panedGetChild2 w)


-- @G: panedGetHandleWindow					 | ["paned","Get","Handle","Window"]
panedGetHandleWindowPassive :: PanedClass self => (self) -> ReactiveFieldRead IO (DrawWindow)
panedGetHandleWindowPassive w = wrapMRPassive (panedGetHandleWindow w)


-- @G: panedGetPosition					 | ["paned","Get","Position"]
panedGetPositionPassive :: PanedClass self => (self) -> ReactiveFieldRead IO (Int)
panedGetPositionPassive w = wrapMRPassive (panedGetPosition w)


-- @A: panedPosition
panedPositionPassive :: PanedClass self => (self) -> ReactiveFieldReadWrite IO (Int)
panedPositionPassive w = passivePropertyNE w panedPosition


-- @A: panedPositionSet
panedPositionSetPassive :: PanedClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
panedPositionSetPassive w = passivePropertyNE w panedPositionSet


-- @T: panedSetPosition					 | ["paned","Set","Position"]
panedSetPositionReactive :: PanedClass self => (self) -> ReactiveFieldWrite IO (Int)
panedSetPositionReactive w = wrapMW (panedSetPosition w)


-- @S: adjustBounds
-- TODO
-- @C: afterAdjustBounds
-- TODO
-- @C: afterMoveSlider
-- TODO
-- @C: afterRangeChangeValue
-- TODO
-- @C: afterRangeValueChanged
afterRangeValueChangedReactive :: RangeClass self => self -> ReactiveFieldRead IO ()
afterRangeValueChangedReactive w = reactivePropertyH_ w afterRangeValueChanged

-- @S: changeValue
-- TODO
-- @C: onAdjustBounds
-- TODO
-- @C: onMoveSlider
-- TODO
-- @C: onRangeChangeValue
-- TODO
-- @C: onRangeValueChanged
onRangeValueChangedReactive :: RangeClass self => self -> ReactiveFieldRead IO ()
onRangeValueChangedReactive w = reactivePropertyH_ w onRangeValueChanged

-- @A: rangeAdjustment
rangeAdjustmentPassive :: RangeClass self => (self) -> ReactiveFieldReadWrite IO (Adjustment)
rangeAdjustmentPassive w = passivePropertyNE w rangeAdjustment


-- @G: rangeGetAdjustment					 | ["range","Get","Adjustment"]
rangeGetAdjustmentPassive :: RangeClass self => (self) -> ReactiveFieldRead IO (Adjustment)
rangeGetAdjustmentPassive w = wrapMRPassive (rangeGetAdjustment w)


-- @G: rangeGetInverted					 | ["range","Get","Inverted"]
rangeGetInvertedPassive :: RangeClass self => (self) -> ReactiveFieldRead IO (Bool)
rangeGetInvertedPassive w = wrapMRPassive (rangeGetInverted w)


-- @G: rangeGetLowerStepperSensitivity					 | ["range","Get","Lower","Stepper","Sensitivity"]
rangeGetLowerStepperSensitivityPassive :: RangeClass self => (self) -> ReactiveFieldRead IO (SensitivityType)
rangeGetLowerStepperSensitivityPassive w = wrapMRPassive (rangeGetLowerStepperSensitivity w)


-- @G: rangeGetMinSliderSize					 | ["range","Get","Min","Slider","Size"]
rangeGetMinSliderSizePassive :: RangeClass self => (self) -> ReactiveFieldRead IO (Int)
rangeGetMinSliderSizePassive w = wrapMRPassive (rangeGetMinSliderSize w)


-- @G: rangeGetRangeRect					 | ["range","Get","Range","Rect"]
rangeGetRangeRectPassive :: RangeClass self => (self) -> ReactiveFieldRead IO (Rectangle)
rangeGetRangeRectPassive w = wrapMRPassive (rangeGetRangeRect w)


-- @G: rangeGetSliderRange					 | ["range","Get","Slider","Range"]
rangeGetSliderRangePassive :: RangeClass self => (self) -> ReactiveFieldRead IO ((Maybe (Int, Int)))
rangeGetSliderRangePassive w = wrapMRPassive (rangeGetSliderRange w)


-- @G: rangeGetSliderSizeFixed					 | ["range","Get","Slider","Size","Fixed"]
rangeGetSliderSizeFixedPassive :: RangeClass self => (self) -> ReactiveFieldRead IO (Bool)
rangeGetSliderSizeFixedPassive w = wrapMRPassive (rangeGetSliderSizeFixed w)


-- @G: rangeGetUpdatePolicy					 | ["range","Get","Update","Policy"]

rangeGetUpdatePolicyPassive w = wrapMRPassive (rangeGetUpdatePolicy w)


-- @G: rangeGetUpperStepperSensitivity					 | ["range","Get","Upper","Stepper","Sensitivity"]
rangeGetUpperStepperSensitivityPassive :: RangeClass self => (self) -> ReactiveFieldRead IO (SensitivityType)
rangeGetUpperStepperSensitivityPassive w = wrapMRPassive (rangeGetUpperStepperSensitivity w)


-- @G: rangeGetValue					 | ["range","Get","Value"]
rangeGetValuePassive :: RangeClass self => (self) -> ReactiveFieldRead IO (Double)
rangeGetValuePassive w = wrapMRPassive (rangeGetValue w)


-- @A: rangeInverted
rangeInvertedPassive :: RangeClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
rangeInvertedPassive w = passivePropertyNE w rangeInverted


-- @A: rangeLowerStepperSensitivity
rangeLowerStepperSensitivityPassive :: RangeClass self => (self) -> ReactiveFieldReadWrite IO (SensitivityType)
rangeLowerStepperSensitivityPassive w = passivePropertyNE w rangeLowerStepperSensitivity


-- @T: rangeSetAdjustment					 | ["range","Set","Adjustment"]
rangeSetAdjustmentReactive :: RangeClass self => (self) -> ReactiveFieldWrite IO (Adjustment)
rangeSetAdjustmentReactive w = wrapMW (rangeSetAdjustment w)


-- @T: rangeSetIncrements					 | ["range","Set","Increments"]
-- TODO
-- @T: rangeSetInverted					 | ["range","Set","Inverted"]
rangeSetInvertedReactive :: RangeClass self => (self) -> ReactiveFieldWrite IO (Bool)
rangeSetInvertedReactive w = wrapMW (rangeSetInverted w)


-- @T: rangeSetLowerStepperSensitivity					 | ["range","Set","Lower","Stepper","Sensitivity"]
rangeSetLowerStepperSensitivityReactive :: RangeClass self => (self) -> ReactiveFieldWrite IO (SensitivityType)
rangeSetLowerStepperSensitivityReactive w = wrapMW (rangeSetLowerStepperSensitivity w)


-- @T: rangeSetMinSliderSize					 | ["range","Set","Min","Slider","Size"]
rangeSetMinSliderSizeReactive :: RangeClass self => (self) -> ReactiveFieldWrite IO (Bool)
rangeSetMinSliderSizeReactive w = wrapMW (rangeSetMinSliderSize w)


-- @T: rangeSetRange					 | ["range","Set","Range"]
-- TODO
-- @T: rangeSetSliderSizeFixed					 | ["range","Set","Slider","Size","Fixed"]
rangeSetSliderSizeFixedReactive :: RangeClass self => (self) -> ReactiveFieldWrite IO (Bool)
rangeSetSliderSizeFixedReactive w = wrapMW (rangeSetSliderSizeFixed w)


-- @T: rangeSetUpdatePolicy					 | ["range","Set","Update","Policy"]

rangeSetUpdatePolicyReactive w = wrapMW (rangeSetUpdatePolicy w)


-- @T: rangeSetUpperStepperSensitivity					 | ["range","Set","Upper","Stepper","Sensitivity"]
rangeSetUpperStepperSensitivityReactive :: RangeClass self => (self) -> ReactiveFieldWrite IO (SensitivityType)
rangeSetUpperStepperSensitivityReactive w = wrapMW (rangeSetUpperStepperSensitivity w)


-- @T: rangeSetValue					 | ["range","Set","Value"]
rangeSetValueReactive :: RangeClass self => (self) -> ReactiveFieldWrite IO (Double)
rangeSetValueReactive w = wrapMW (rangeSetValue w)


-- @A: rangeSliderSizeFixed
rangeSliderSizeFixedPassive :: RangeClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
rangeSliderSizeFixedPassive w = passivePropertyNE w rangeSliderSizeFixed


-- @A: rangeUpdatePolicy

rangeUpdatePolicyPassive w = passivePropertyNE w rangeUpdatePolicy


-- @A: rangeUpperStepperSensitivity
rangeUpperStepperSensitivityPassive :: RangeClass self => (self) -> ReactiveFieldReadWrite IO (SensitivityType)
rangeUpperStepperSensitivityPassive w = passivePropertyNE w rangeUpperStepperSensitivity


-- @A: rangeValue
rangeValuePassive :: RangeClass self => (self) -> ReactiveFieldReadWrite IO (Double)
rangeValuePassive w = passivePropertyNE w rangeValue


-- @S: valueChanged
valueChangedReactive :: RangeClass self => self -> ReactiveFieldRead IO ()
valueChangedReactive = (`reactiveSignalIO` valueChanged)


-- @A: scaleDigits
scaleDigitsPassive :: ScaleClass self => (self) -> ReactiveFieldReadWrite IO (Int)
scaleDigitsPassive w = passivePropertyNE w scaleDigits


-- @A: scaleDrawValue
scaleDrawValuePassive :: ScaleClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
scaleDrawValuePassive w = passivePropertyNE w scaleDrawValue


-- @G: scaleGetDigits					 | ["scale","Get","Digits"]
scaleGetDigitsPassive :: ScaleClass self => (self) -> ReactiveFieldRead IO (Int)
scaleGetDigitsPassive w = wrapMRPassive (scaleGetDigits w)


-- @G: scaleGetDrawValue					 | ["scale","Get","Draw","Value"]
scaleGetDrawValuePassive :: ScaleClass self => (self) -> ReactiveFieldRead IO (Bool)
scaleGetDrawValuePassive w = wrapMRPassive (scaleGetDrawValue w)


-- @G: scaleGetValuePos					 | ["scale","Get","Value","Pos"]
scaleGetValuePosPassive :: ScaleClass self => (self) -> ReactiveFieldRead IO (PositionType)
scaleGetValuePosPassive w = wrapMRPassive (scaleGetValuePos w)


-- @T: scaleSetDigits					 | ["scale","Set","Digits"]
scaleSetDigitsReactive :: ScaleClass self => (self) -> ReactiveFieldWrite IO (Int)
scaleSetDigitsReactive w = wrapMW (scaleSetDigits w)


-- @T: scaleSetDrawValue					 | ["scale","Set","Draw","Value"]
scaleSetDrawValueReactive :: ScaleClass self => (self) -> ReactiveFieldWrite IO (Bool)
scaleSetDrawValueReactive w = wrapMW (scaleSetDrawValue w)


-- @T: scaleSetValuePos					 | ["scale","Set","Value","Pos"]
scaleSetValuePosReactive :: ScaleClass self => (self) -> ReactiveFieldWrite IO (PositionType)
scaleSetValuePosReactive w = wrapMW (scaleSetValuePos w)


-- @A: scaleValuePos
scaleValuePosPassive :: ScaleClass self => (self) -> ReactiveFieldReadWrite IO (PositionType)
scaleValuePosPassive w = passivePropertyNE w scaleValuePos


-- @S: accelClosuresChanged
accelClosuresChangedReactive :: WidgetClass self => self -> ReactiveFieldRead IO ()
accelClosuresChangedReactive = (`reactiveSignalIO` accelClosuresChanged)


-- @C: afterButtonPress
-- TODO
-- @C: afterButtonRelease
-- TODO
-- @C: afterClient
-- TODO
-- @C: afterConfigure
-- TODO
-- @C: afterDelete
-- TODO
-- @C: afterDestroyEvent
-- TODO
-- @C: afterDestroy
afterDestroyReactive :: WidgetClass self => self -> ReactiveFieldRead IO ()
afterDestroyReactive w = reactivePropertyH_ w afterDestroy

-- @C: afterDirectionChanged
-- TODO
-- @C: afterEnterNotify
-- TODO
-- @C: afterExpose
-- TODO
-- @C: afterExposeRect
-- TODO
-- @C: afterFocus
-- TODO
-- @C: afterFocusIn
-- TODO
-- @C: afterFocusOut
-- TODO
-- @C: afterGrabFocus
afterGrabFocusReactive :: WidgetClass self => self -> ReactiveFieldRead IO ()
afterGrabFocusReactive w = reactivePropertyH_ w afterGrabFocus

-- @C: afterHide
afterHideReactive :: WidgetClass self => self -> ReactiveFieldRead IO ()
afterHideReactive w = reactivePropertyH_ w afterHide

-- @C: afterHierarchyChanged
afterHierarchyChangedReactive :: WidgetClass self => self -> ReactiveFieldRead IO ()
afterHierarchyChangedReactive w = reactivePropertyH_ w afterHierarchyChanged

-- @C: afterKeyPress
-- TODO
-- @C: afterKeyRelease
-- TODO
-- @C: afterLeaveNotify
-- TODO
-- @C: afterMnemonicActivate
-- TODO
-- @C: afterMotionNotify
-- TODO
-- @C: afterParentSet
-- TODO
-- @C: afterPopupMenu
afterPopupMenuReactive :: WidgetClass self => self -> ReactiveFieldRead IO ()
afterPopupMenuReactive w = reactivePropertyH_ w afterPopupMenu

-- @C: afterProximityIn
-- TODO
-- @C: afterProximityOut
-- TODO
-- @C: afterRealize
afterRealizeReactive :: WidgetClass self => self -> ReactiveFieldRead IO ()
afterRealizeReactive w = reactivePropertyH_ w afterRealize

-- @C: afterScroll
-- TODO
-- @C: afterShow
afterShowReactive :: WidgetClass self => self -> ReactiveFieldRead IO ()
afterShowReactive w = reactivePropertyH_ w afterShow

-- @C: afterSizeAllocate
-- TODO
-- @C: afterSizeRequest
-- TODO
-- @C: afterStateChanged
-- TODO
-- @C: afterUnmap
afterUnmapReactive :: WidgetClass self => self -> ReactiveFieldRead IO ()
afterUnmapReactive w = reactivePropertyH_ w afterUnmap

-- @C: afterUnrealize
afterUnrealizeReactive :: WidgetClass self => self -> ReactiveFieldRead IO ()
afterUnrealizeReactive w = reactivePropertyH_ w afterUnrealize

-- @C: afterVisibilityNotify
-- TODO
-- @C: afterWindowState
-- TODO
-- @S: buttonPressEvent
-- TODO
-- @S: buttonReleaseEvent
-- TODO
-- @S: configureEvent
-- TODO
-- @S: deleteEvent
-- TODO
-- @S: destroyEvent
-- TODO
-- @S: directionChanged
-- TODO
-- @S: enterNotifyEvent
-- TODO
-- @S: exposeEvent
-- TODO
-- @S: focus
-- TODO
-- @S: focusInEvent
-- TODO
-- @S: focusOutEvent
-- TODO
-- @S: grabBrokenEvent
-- TODO
-- @S: grabNotify
-- TODO
-- @S: hideSignal
hideSignalReactive :: WidgetClass self => self -> ReactiveFieldRead IO ()
hideSignalReactive = (`reactiveSignalIO` hideSignal)


-- @S: hierarchyChanged
-- TODO
-- @S: keyPressEvent
-- TODO
-- @S: keyReleaseEvent
-- TODO
-- @S: leaveNotifyEvent
-- TODO
-- @S: mapEvent
-- TODO
-- @S: mapSignal
mapSignalReactive :: WidgetClass self => self -> ReactiveFieldRead IO ()
mapSignalReactive = (`reactiveSignalIO` mapSignal)


-- @S: motionNotifyEvent
-- TODO
-- @S: noExposeEvent
-- TODO
-- @C: onButtonPress
-- TODO
-- @C: onButtonRelease
-- TODO
-- @C: onClient
-- TODO
-- @C: onConfigure
-- TODO
-- @C: onDelete
-- TODO
-- @C: onDestroyEvent
-- TODO
-- @C: onDestroy
onDestroyReactive :: WidgetClass self => self -> ReactiveFieldRead IO ()
onDestroyReactive w = reactivePropertyH_ w onDestroy

-- @C: onDirectionChanged
-- TODO
-- @C: onEnterNotify
-- TODO
-- @C: onExpose
-- TODO
-- @C: onExposeRect
-- TODO
-- @C: onFocus
-- TODO
-- @C: onFocusIn
-- TODO
-- @C: onFocusOut
-- TODO
-- @C: onGrabFocus
onGrabFocusReactive :: WidgetClass self => self -> ReactiveFieldRead IO ()
onGrabFocusReactive w = reactivePropertyH_ w onGrabFocus

-- @C: onHide
onHideReactive :: WidgetClass self => self -> ReactiveFieldRead IO ()
onHideReactive w = reactivePropertyH_ w onHide

-- @C: onHierarchyChanged
onHierarchyChangedReactive :: WidgetClass self => self -> ReactiveFieldRead IO ()
onHierarchyChangedReactive w = reactivePropertyH_ w onHierarchyChanged

-- @C: onKeyPress
-- TODO
-- @C: onKeyRelease
-- TODO
-- @C: onLeaveNotify
-- TODO
-- @C: onMnemonicActivate
-- TODO
-- @C: onMotionNotify
-- TODO
-- @C: onParentSet
-- TODO
-- @C: onPopupMenu
onPopupMenuReactive :: WidgetClass self => self -> ReactiveFieldRead IO ()
onPopupMenuReactive w = reactivePropertyH_ w onPopupMenu

-- @C: onProximityIn
-- TODO
-- @C: onProximityOut
-- TODO
-- @C: onRealize
onRealizeReactive :: WidgetClass self => self -> ReactiveFieldRead IO ()
onRealizeReactive w = reactivePropertyH_ w onRealize

-- @C: onScroll
-- TODO
-- @C: onShow
onShowReactive :: WidgetClass self => self -> ReactiveFieldRead IO ()
onShowReactive w = reactivePropertyH_ w onShow

-- @C: onSizeAllocate
-- TODO
-- @C: onSizeRequest
-- TODO
-- @C: onStateChanged
-- TODO
-- @C: onUnmap
onUnmapReactive :: WidgetClass self => self -> ReactiveFieldRead IO ()
onUnmapReactive w = reactivePropertyH_ w onUnmap

-- @C: onUnrealize
onUnrealizeReactive :: WidgetClass self => self -> ReactiveFieldRead IO ()
onUnrealizeReactive w = reactivePropertyH_ w onUnrealize

-- @C: onVisibilityNotify
-- TODO
-- @C: onWindowState
-- TODO
-- @S: parentSet
-- TODO
-- @S: popupMenuSignal
-- TODO
-- @S: proximityInEvent
-- TODO
-- @S: proximityOutEvent
-- TODO
-- @S: queryTooltip
-- TODO
-- @S: realize
realizeReactive :: WidgetClass self => self -> ReactiveFieldRead IO ()
realizeReactive = (`reactiveSignalIO` realize)


-- @S: screenChanged
-- TODO
-- @S: scrollEvent
-- TODO
-- @S: showHelp
-- TODO
-- @S: showSignal
showSignalReactive :: WidgetClass self => self -> ReactiveFieldRead IO ()
showSignalReactive = (`reactiveSignalIO` showSignal)


-- @S: sizeAllocate
-- TODO
-- @S: sizeRequest
-- TODO
-- @S: stateChanged
-- TODO
-- @S: styleSet
-- TODO
-- @S: unmapEvent
-- TODO
-- @S: unmapSignal
unmapSignalReactive :: WidgetClass self => self -> ReactiveFieldRead IO ()
unmapSignalReactive = (`reactiveSignalIO` unmapSignal)


-- @S: unrealize
unrealizeReactive :: WidgetClass self => self -> ReactiveFieldRead IO ()
unrealizeReactive = (`reactiveSignalIO` unrealize)


-- @S: visibilityNotifyEvent
-- TODO
-- @A: widgetAppPaintable
widgetAppPaintablePassive :: WidgetClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
widgetAppPaintablePassive w = passivePropertyNE w widgetAppPaintable


-- @A: widgetCanDefault
widgetCanDefaultPassive :: WidgetClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
widgetCanDefaultPassive w = passivePropertyNE w widgetCanDefault


-- @A: widgetCanFocus
widgetCanFocusPassive :: WidgetClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
widgetCanFocusPassive w = passivePropertyNE w widgetCanFocus


-- @A: widgetChildVisible
widgetChildVisiblePassive :: WidgetClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
widgetChildVisiblePassive w = passivePropertyNE w widgetChildVisible


-- @A: widgetColormap

widgetColormapPassive w = passivePropertyNE w widgetColormap


-- @A: widgetDirection
widgetDirectionPassive :: WidgetClass self => (self) -> ReactiveFieldReadWrite IO (TextDirection)
widgetDirectionPassive w = passivePropertyNE w widgetDirection


-- @A: widgetEvents
widgetEventsPassive :: WidgetClass self => (self) -> ReactiveFieldReadWrite IO ([EventMask])
widgetEventsPassive w = passivePropertyNE w widgetEvents


-- @A: widgetExpand
widgetExpandPassive :: WidgetClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
widgetExpandPassive w = passivePropertyNE w widgetExpand


-- @A: widgetExtensionEvents
widgetExtensionEventsPassive :: WidgetClass self => (self) -> ReactiveFieldReadWrite IO ([ExtensionMode])
widgetExtensionEventsPassive w = passivePropertyNE w widgetExtensionEvents


-- @G: widgetGetAccessible					 | ["widget","Get","Accessible"]
widgetGetAccessiblePassive :: WidgetClass self => (self) -> ReactiveFieldRead IO (Object)
widgetGetAccessiblePassive w = wrapMRPassive (widgetGetAccessible w)


-- @G: widgetGetAction					 | ["widget","Get","Action"]
widgetGetActionPassive :: WidgetClass self => (self) -> ReactiveFieldRead IO ((Maybe Action))
widgetGetActionPassive w = wrapMRPassive (widgetGetAction w)


-- @G: widgetGetAllocation					 | ["widget","Get","Allocation"]
widgetGetAllocationPassive :: WidgetClass self => (self) -> ReactiveFieldRead IO (Allocation)
widgetGetAllocationPassive w = wrapMRPassive (widgetGetAllocation w)


-- @G: widgetGetAncestor					 | ["widget","Get","Ancestor"]
-- TODO
-- @G: widgetGetAppPaintable					 | ["widget","Get","App","Paintable"]
widgetGetAppPaintablePassive :: WidgetClass widget => (widget) -> ReactiveFieldRead IO (Bool)
widgetGetAppPaintablePassive w = wrapMRPassive (widgetGetAppPaintable w)


-- @G: widgetGetCanDefault					 | ["widget","Get","Can","Default"]
widgetGetCanDefaultPassive :: WidgetClass widget => (widget) -> ReactiveFieldRead IO (Bool)
widgetGetCanDefaultPassive w = wrapMRPassive (widgetGetCanDefault w)


-- @G: widgetGetCanFocus					 | ["widget","Get","Can","Focus"]
widgetGetCanFocusPassive :: WidgetClass self => (self) -> ReactiveFieldRead IO (Bool)
widgetGetCanFocusPassive w = wrapMRPassive (widgetGetCanFocus w)


-- @G: widgetGetChildRequisition					 | ["widget","Get","Child","Requisition"]
widgetGetChildRequisitionPassive :: WidgetClass self => (self) -> ReactiveFieldRead IO (Requisition)
widgetGetChildRequisitionPassive w = wrapMRPassive (widgetGetChildRequisition w)


-- @G: widgetGetChildVisible					 | ["widget","Get","Child","Visible"]
widgetGetChildVisiblePassive :: WidgetClass self => (self) -> ReactiveFieldRead IO (Bool)
widgetGetChildVisiblePassive w = wrapMRPassive (widgetGetChildVisible w)


-- @G: widgetGetClipboard					 | ["widget","Get","Clipboard"]
-- TODO
-- @G: widgetGetColormap					 | ["widget","Get","Colormap"]

widgetGetColormapPassive w = wrapMRPassive (widgetGetColormap w)


-- @G: widgetGetCompositeName					 | ["widget","Get","Composite","Name"]
widgetGetCompositeNamePassive :: (WidgetClass self, GlibString string) => (self) -> ReactiveFieldRead IO ((Maybe string))
widgetGetCompositeNamePassive w = wrapMRPassive (widgetGetCompositeName w)


-- @G: widgetGetDefaultColormap					 | ["widget","Get","Default","Colormap"]

widgetGetDefaultColormapPassive = wrapMRPassive (widgetGetDefaultColormap)


-- @G: widgetGetDefaultDirection					 | ["widget","Get","Default","Direction"]
widgetGetDefaultDirectionPassive :: ReactiveFieldRead IO (TextDirection)
widgetGetDefaultDirectionPassive = wrapMRPassive (widgetGetDefaultDirection)


-- @G: widgetGetDefaultStyle					 | ["widget","Get","Default","Style"]
widgetGetDefaultStylePassive :: ReactiveFieldRead IO (Style)
widgetGetDefaultStylePassive = wrapMRPassive (widgetGetDefaultStyle)


-- @G: widgetGetDirection					 | ["widget","Get","Direction"]
widgetGetDirectionPassive :: WidgetClass self => (self) -> ReactiveFieldRead IO (TextDirection)
widgetGetDirectionPassive w = wrapMRPassive (widgetGetDirection w)


-- @G: widgetGetDisplay					 | ["widget","Get","Display"]
widgetGetDisplayPassive :: WidgetClass self => (self) -> ReactiveFieldRead IO (Display)
widgetGetDisplayPassive w = wrapMRPassive (widgetGetDisplay w)


-- @G: widgetGetEvents					 | ["widget","Get","Events"]
widgetGetEventsPassive :: WidgetClass self => (self) -> ReactiveFieldRead IO ([EventMask])
widgetGetEventsPassive w = wrapMRPassive (widgetGetEvents w)


-- @G: widgetGetExtensionEvents					 | ["widget","Get","Extension","Events"]
widgetGetExtensionEventsPassive :: WidgetClass self => (self) -> ReactiveFieldRead IO ([ExtensionMode])
widgetGetExtensionEventsPassive w = wrapMRPassive (widgetGetExtensionEvents w)


-- @G: widgetGetHasDefault					 | ["widget","Get","Has","Default"]
widgetGetHasDefaultPassive :: WidgetClass widget => (widget) -> ReactiveFieldRead IO (Bool)
widgetGetHasDefaultPassive w = wrapMRPassive (widgetGetHasDefault w)


-- @G: widgetGetHasFocus					 | ["widget","Get","Has","Focus"]
widgetGetHasFocusPassive :: WidgetClass widget => (widget) -> ReactiveFieldRead IO (Bool)
widgetGetHasFocusPassive w = wrapMRPassive (widgetGetHasFocus w)


-- @G: widgetGetHasTooltip					 | ["widget","Get","Has","Tooltip"]
widgetGetHasTooltipPassive :: WidgetClass widget => (widget) -> ReactiveFieldRead IO (Bool)
widgetGetHasTooltipPassive w = wrapMRPassive (widgetGetHasTooltip w)


-- @G: widgetGetHasWindow					 | ["widget","Get","Has","Window"]
widgetGetHasWindowPassive :: WidgetClass widget => (widget) -> ReactiveFieldRead IO (Bool)
widgetGetHasWindowPassive w = wrapMRPassive (widgetGetHasWindow w)


-- @G: widgetGetIsFocus					 | ["widget","Get","Is","Focus"]
widgetGetIsFocusPassive :: WidgetClass self => (self) -> ReactiveFieldRead IO (Bool)
widgetGetIsFocusPassive w = wrapMRPassive (widgetGetIsFocus w)


-- @G: widgetGetMapped					 | ["widget","Get","Mapped"]
widgetGetMappedPassive :: WidgetClass self => (self) -> ReactiveFieldRead IO (Bool)
widgetGetMappedPassive w = wrapMRPassive (widgetGetMapped w)


-- @G: widgetGetModifierStyle					 | ["widget","Get","Modifier","Style"]
widgetGetModifierStylePassive :: WidgetClass self => (self) -> ReactiveFieldRead IO (RcStyle)
widgetGetModifierStylePassive w = wrapMRPassive (widgetGetModifierStyle w)


-- @G: widgetGetName					 | ["widget","Get","Name"]
widgetGetNamePassive :: (WidgetClass self, GlibString string) => (self) -> ReactiveFieldRead IO (string)
widgetGetNamePassive w = wrapMRPassive (widgetGetName w)


-- @G: widgetGetNoShowAll					 | ["widget","Get","No","Show","All"]
widgetGetNoShowAllPassive :: WidgetClass self => (self) -> ReactiveFieldRead IO (Bool)
widgetGetNoShowAllPassive w = wrapMRPassive (widgetGetNoShowAll w)


-- @G: widgetGetPangoContext					 | ["widget","Get","Pango","Context"]
widgetGetPangoContextPassive :: WidgetClass self => (self) -> ReactiveFieldRead IO (PangoContext)
widgetGetPangoContextPassive w = wrapMRPassive (widgetGetPangoContext w)


-- @G: widgetGetParent					 | ["widget","Get","Parent"]
widgetGetParentPassive :: WidgetClass self => (self) -> ReactiveFieldRead IO ((Maybe Widget))
widgetGetParentPassive w = wrapMRPassive (widgetGetParent w)


-- @G: widgetGetParentWindow					 | ["widget","Get","Parent","Window"]
widgetGetParentWindowPassive :: WidgetClass self => (self) -> ReactiveFieldRead IO (DrawWindow)
widgetGetParentWindowPassive w = wrapMRPassive (widgetGetParentWindow w)


-- @G: widgetGetPointer					 | ["widget","Get","Pointer"]
widgetGetPointerPassive :: WidgetClass self => (self) -> ReactiveFieldRead IO ((Int, Int))
widgetGetPointerPassive w = wrapMRPassive (widgetGetPointer w)


-- @G: widgetGetRealized					 | ["widget","Get","Realized"]
widgetGetRealizedPassive :: WidgetClass self => (self) -> ReactiveFieldRead IO (Bool)
widgetGetRealizedPassive w = wrapMRPassive (widgetGetRealized w)


-- @G: widgetGetReceivesDefault					 | ["widget","Get","Receives","Default"]
widgetGetReceivesDefaultPassive :: WidgetClass widget => (widget) -> ReactiveFieldRead IO (Bool)
widgetGetReceivesDefaultPassive w = wrapMRPassive (widgetGetReceivesDefault w)


-- @G: widgetGetRootWindow					 | ["widget","Get","Root","Window"]
widgetGetRootWindowPassive :: WidgetClass self => (self) -> ReactiveFieldRead IO (DrawWindow)
widgetGetRootWindowPassive w = wrapMRPassive (widgetGetRootWindow w)


-- @G: widgetGetScreen					 | ["widget","Get","Screen"]
widgetGetScreenPassive :: WidgetClass self => (self) -> ReactiveFieldRead IO (Screen)
widgetGetScreenPassive w = wrapMRPassive (widgetGetScreen w)


-- @G: widgetGetSensitive					 | ["widget","Get","Sensitive"]
widgetGetSensitivePassive :: WidgetClass widget => (widget) -> ReactiveFieldRead IO (Bool)
widgetGetSensitivePassive w = wrapMRPassive (widgetGetSensitive w)


-- @G: widgetGetSettings					 | ["widget","Get","Settings"]
widgetGetSettingsPassive :: WidgetClass self => (self) -> ReactiveFieldRead IO (Settings)
widgetGetSettingsPassive w = wrapMRPassive (widgetGetSettings w)


-- @G: widgetGetSizeRequest					 | ["widget","Get","Size","Request"]
widgetGetSizeRequestPassive :: WidgetClass self => (self) -> ReactiveFieldRead IO ((Int, Int))
widgetGetSizeRequestPassive w = wrapMRPassive (widgetGetSizeRequest w)


-- @G: widgetGetSnapshot					 | ["widget","Get","Snapshot"]
-- TODO
-- @G: widgetGetState					 | ["widget","Get","State"]
widgetGetStatePassive :: WidgetClass self => (self) -> ReactiveFieldRead IO (StateType)
widgetGetStatePassive w = wrapMRPassive (widgetGetState w)


-- @G: widgetGetStyle					 | ["widget","Get","Style"]
widgetGetStylePassive :: WidgetClass widget => (widget) -> ReactiveFieldRead IO (Style)
widgetGetStylePassive w = wrapMRPassive (widgetGetStyle w)


-- @G: widgetGetTooltipMarkup					 | ["widget","Get","Tooltip","Markup"]
widgetGetTooltipMarkupPassive :: (WidgetClass self, GlibString markup) => (self) -> ReactiveFieldRead IO ((Maybe markup))
widgetGetTooltipMarkupPassive w = wrapMRPassive (widgetGetTooltipMarkup w)


-- @G: widgetGetTooltipText					 | ["widget","Get","Tooltip","Text"]
widgetGetTooltipTextPassive :: (WidgetClass self, GlibString text) => (self) -> ReactiveFieldRead IO ((Maybe text))
widgetGetTooltipTextPassive w = wrapMRPassive (widgetGetTooltipText w)


-- @G: widgetGetTooltipWindow					 | ["widget","Get","Tooltip","Window"]
widgetGetTooltipWindowPassive :: WidgetClass self => (self) -> ReactiveFieldRead IO (Window)
widgetGetTooltipWindowPassive w = wrapMRPassive (widgetGetTooltipWindow w)


-- @G: widgetGetToplevel					 | ["widget","Get","Toplevel"]
widgetGetToplevelPassive :: WidgetClass self => (self) -> ReactiveFieldRead IO (Widget)
widgetGetToplevelPassive w = wrapMRPassive (widgetGetToplevel w)


-- @G: widgetGetVisible					 | ["widget","Get","Visible"]
widgetGetVisiblePassive :: WidgetClass widget => (widget) -> ReactiveFieldRead IO (Bool)
widgetGetVisiblePassive w = wrapMRPassive (widgetGetVisible w)


-- @G: widgetGetWindow					 | ["widget","Get","Window"]
widgetGetWindowPassive :: WidgetClass self => (self) -> ReactiveFieldRead IO ((Maybe DrawWindow))
widgetGetWindowPassive w = wrapMRPassive (widgetGetWindow w)


-- @A: widgetHasDefault
widgetHasDefaultPassive :: WidgetClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
widgetHasDefaultPassive w = passivePropertyNE w widgetHasDefault


-- @A: widgetHasFocus
widgetHasFocusPassive :: WidgetClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
widgetHasFocusPassive w = passivePropertyNE w widgetHasFocus


-- @A: widgetHasTooltip
widgetHasTooltipPassive :: WidgetClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
widgetHasTooltipPassive w = passivePropertyNE w widgetHasTooltip


-- @A: widgetHeightRequest
widgetHeightRequestPassive :: WidgetClass self => (self) -> ReactiveFieldReadWrite IO (Int)
widgetHeightRequestPassive w = passivePropertyNE w widgetHeightRequest


-- @A: widgetHExpand
widgetHExpandPassive :: WidgetClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
widgetHExpandPassive w = passivePropertyNE w widgetHExpand


-- @A: widgetHExpandSet
widgetHExpandSetPassive :: WidgetClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
widgetHExpandSetPassive w = passivePropertyNE w widgetHExpandSet


-- @A: widgetIsFocus
widgetIsFocusPassive :: WidgetClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
widgetIsFocusPassive w = passivePropertyNE w widgetIsFocus


-- @A: widgetMarginBottom
widgetMarginBottomPassive :: WidgetClass self => (self) -> ReactiveFieldReadWrite IO (Int)
widgetMarginBottomPassive w = passivePropertyNE w widgetMarginBottom


-- @A: widgetMarginLeft
widgetMarginLeftPassive :: WidgetClass self => (self) -> ReactiveFieldReadWrite IO (Int)
widgetMarginLeftPassive w = passivePropertyNE w widgetMarginLeft


-- @A: widgetMarginRight
widgetMarginRightPassive :: WidgetClass self => (self) -> ReactiveFieldReadWrite IO (Int)
widgetMarginRightPassive w = passivePropertyNE w widgetMarginRight


-- @A: widgetMarginTop
widgetMarginTopPassive :: WidgetClass self => (self) -> ReactiveFieldReadWrite IO (Int)
widgetMarginTopPassive w = passivePropertyNE w widgetMarginTop


-- @A: widgetName
widgetNamePassive :: (WidgetClass self, GlibString string) => (self) -> ReactiveFieldReadWrite IO ((Maybe string))
widgetNamePassive w = passivePropertyNE w widgetName


-- @A: widgetNoShowAll
widgetNoShowAllPassive :: WidgetClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
widgetNoShowAllPassive w = passivePropertyNE w widgetNoShowAll


-- @A: widgetOpacity
widgetOpacityPassive :: WidgetClass self => (self) -> ReactiveFieldReadWrite IO (Double)
widgetOpacityPassive w = passivePropertyNE w widgetOpacity


-- @A: widgetReceivesDefault
widgetReceivesDefaultPassive :: WidgetClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
widgetReceivesDefaultPassive w = passivePropertyNE w widgetReceivesDefault


-- @A: widgetSensitive
widgetSensitivePassive :: WidgetClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
widgetSensitivePassive w = passivePropertyNE w widgetSensitive


-- @T: widgetSetAccelPath					 | ["widget","Set","Accel","Path"]
-- TODO
-- @T: widgetSetAppPaintable					 | ["widget","Set","App","Paintable"]
widgetSetAppPaintableReactive :: WidgetClass self => (self) -> ReactiveFieldWrite IO (Bool)
widgetSetAppPaintableReactive w = wrapMW (widgetSetAppPaintable w)


-- @T: widgetSetCanDefault					 | ["widget","Set","Can","Default"]
widgetSetCanDefaultReactive :: WidgetClass widget => (widget) -> ReactiveFieldWrite IO (Bool)
widgetSetCanDefaultReactive w = wrapMW (widgetSetCanDefault w)


-- @T: widgetSetCanFocus					 | ["widget","Set","Can","Focus"]
widgetSetCanFocusReactive :: WidgetClass self => (self) -> ReactiveFieldWrite IO (Bool)
widgetSetCanFocusReactive w = wrapMW (widgetSetCanFocus w)


-- @T: widgetSetChildVisible					 | ["widget","Set","Child","Visible"]
widgetSetChildVisibleReactive :: WidgetClass self => (self) -> ReactiveFieldWrite IO (Bool)
widgetSetChildVisibleReactive w = wrapMW (widgetSetChildVisible w)


-- @T: widgetSetColormap					 | ["widget","Set","Colormap"]

widgetSetColormapReactive w = wrapMW (widgetSetColormap w)


-- @T: widgetSetCompositeName					 | ["widget","Set","Composite","Name"]
widgetSetCompositeNameReactive :: (WidgetClass self, GlibString string) => (self) -> ReactiveFieldWrite IO (string)
widgetSetCompositeNameReactive w = wrapMW (widgetSetCompositeName w)


-- @T: widgetSetDefaultColormap					 | ["widget","Set","Default","Colormap"]
-- TODO
-- @T: widgetSetDefaultDirection					 | ["widget","Set","Default","Direction"]
-- TODO
-- @T: widgetSetDirection					 | ["widget","Set","Direction"]
widgetSetDirectionReactive :: WidgetClass self => (self) -> ReactiveFieldWrite IO (TextDirection)
widgetSetDirectionReactive w = wrapMW (widgetSetDirection w)


-- @T: widgetSetDoubleBuffered					 | ["widget","Set","Double","Buffered"]
widgetSetDoubleBufferedReactive :: WidgetClass self => (self) -> ReactiveFieldWrite IO (Bool)
widgetSetDoubleBufferedReactive w = wrapMW (widgetSetDoubleBuffered w)


-- @T: widgetSetEvents					 | ["widget","Set","Events"]
widgetSetEventsReactive :: WidgetClass self => (self) -> ReactiveFieldWrite IO ([EventMask])
widgetSetEventsReactive w = wrapMW (widgetSetEvents w)


-- @T: widgetSetExtensionEvents					 | ["widget","Set","Extension","Events"]
widgetSetExtensionEventsReactive :: WidgetClass self => (self) -> ReactiveFieldWrite IO ([ExtensionMode])
widgetSetExtensionEventsReactive w = wrapMW (widgetSetExtensionEvents w)


-- @T: widgetSetHasTooltip					 | ["widget","Set","Has","Tooltip"]
widgetSetHasTooltipReactive :: WidgetClass widget => (widget) -> ReactiveFieldWrite IO (Bool)
widgetSetHasTooltipReactive w = wrapMW (widgetSetHasTooltip w)


-- @T: widgetSetHasWindow					 | ["widget","Set","Has","Window"]
widgetSetHasWindowReactive :: WidgetClass widget => (widget) -> ReactiveFieldWrite IO (Bool)
widgetSetHasWindowReactive w = wrapMW (widgetSetHasWindow w)


-- @T: widgetSetMapped					 | ["widget","Set","Mapped"]
widgetSetMappedReactive :: WidgetClass widget => (widget) -> ReactiveFieldWrite IO (Bool)
widgetSetMappedReactive w = wrapMW (widgetSetMapped w)


-- @T: widgetSetName					 | ["widget","Set","Name"]
widgetSetNameReactive :: (WidgetClass self, GlibString string) => (self) -> ReactiveFieldWrite IO (string)
widgetSetNameReactive w = wrapMW (widgetSetName w)


-- @T: widgetSetNoShowAll					 | ["widget","Set","No","Show","All"]
widgetSetNoShowAllReactive :: WidgetClass self => (self) -> ReactiveFieldWrite IO (Bool)
widgetSetNoShowAllReactive w = wrapMW (widgetSetNoShowAll w)


-- @T: widgetSetRealized					 | ["widget","Set","Realized"]
widgetSetRealizedReactive :: WidgetClass widget => (widget) -> ReactiveFieldWrite IO (Bool)
widgetSetRealizedReactive w = wrapMW (widgetSetRealized w)


-- @T: widgetSetReceivesDefault					 | ["widget","Set","Receives","Default"]
widgetSetReceivesDefaultReactive :: WidgetClass widget => (widget) -> ReactiveFieldWrite IO (Bool)
widgetSetReceivesDefaultReactive w = wrapMW (widgetSetReceivesDefault w)


-- @T: widgetSetRedrawOnAllocate					 | ["widget","Set","Redraw","On","Allocate"]
widgetSetRedrawOnAllocateReactive :: WidgetClass self => (self) -> ReactiveFieldWrite IO (Bool)
widgetSetRedrawOnAllocateReactive w = wrapMW (widgetSetRedrawOnAllocate w)


-- @T: widgetSetScrollAdjustments					 | ["widget","Set","Scroll","Adjustments"]
-- TODO
-- @T: widgetSetSensitive					 | ["widget","Set","Sensitive"]
widgetSetSensitiveReactive :: WidgetClass self => (self) -> ReactiveFieldWrite IO (Bool)
widgetSetSensitiveReactive w = wrapMW (widgetSetSensitive w)


-- @T: widgetSetSensitivity					 | ["widget","Set","Sensitivity"]
widgetSetSensitivityReactive :: WidgetClass self => (self) -> ReactiveFieldWrite IO (Bool)
widgetSetSensitivityReactive w = wrapMW (widgetSetSensitivity w)


-- @T: widgetSetSizeRequest					 | ["widget","Set","Size","Request"]
-- TODO
-- @T: widgetSetState					 | ["widget","Set","State"]
widgetSetStateReactive :: WidgetClass self => (self) -> ReactiveFieldWrite IO (StateType)
widgetSetStateReactive w = wrapMW (widgetSetState w)


-- @T: widgetSetStyle					 | ["widget","Set","Style"]
widgetSetStyleReactive :: WidgetClass self => (self) -> ReactiveFieldWrite IO (Maybe Style)
widgetSetStyleReactive w = wrapMW (widgetSetStyle w)


-- @T: widgetSetTooltipMarkup					 | ["widget","Set","Tooltip","Markup"]
widgetSetTooltipMarkupReactive :: (WidgetClass self, GlibString markup) => (self) -> ReactiveFieldWrite IO (Maybe markup)
widgetSetTooltipMarkupReactive w = wrapMW (widgetSetTooltipMarkup w)


-- @T: widgetSetTooltipText					 | ["widget","Set","Tooltip","Text"]
widgetSetTooltipTextReactive :: (WidgetClass widget, GlibString text) => (widget) -> ReactiveFieldWrite IO (Maybe text)
widgetSetTooltipTextReactive w = wrapMW (widgetSetTooltipText w)


-- @T: widgetSetTooltipWindow					 | ["widget","Set","Tooltip","Window"]
widgetSetTooltipWindowReactive :: (WidgetClass self, WindowClass customWindow) => (self) -> ReactiveFieldWrite IO (Maybe customWindow)
widgetSetTooltipWindowReactive w = wrapMW (widgetSetTooltipWindow w)


-- @T: widgetSetWindow					 | ["widget","Set","Window"]
widgetSetWindowReactive :: (WidgetClass widget, DrawWindowClass window) => (widget) -> ReactiveFieldWrite IO (window)
widgetSetWindowReactive w = wrapMW (widgetSetWindow w)


-- @A: widgetState
widgetStatePassive :: WidgetClass self => (self) -> ReactiveFieldReadWrite IO (StateType)
widgetStatePassive w = passivePropertyNE w widgetState


-- @A: widgetStyle
widgetStylePassive :: WidgetClass self => (self) -> ReactiveFieldReadWrite IO (Style)
widgetStylePassive w = passivePropertyNE w widgetStyle


-- @A: widgetTooltipMarkup
widgetTooltipMarkupPassive :: (WidgetClass self, GlibString markup) => (self) -> ReactiveFieldReadWrite IO ((Maybe markup))
widgetTooltipMarkupPassive w = passivePropertyNE w widgetTooltipMarkup


-- @A: widgetTooltipText
widgetTooltipTextPassive :: (WidgetClass self, GlibString string) => (self) -> ReactiveFieldReadWrite IO ((Maybe string))
widgetTooltipTextPassive w = passivePropertyNE w widgetTooltipText


-- @A: widgetVExpand
widgetVExpandPassive :: WidgetClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
widgetVExpandPassive w = passivePropertyNE w widgetVExpand


-- @A: widgetVExpandSet
widgetVExpandSetPassive :: WidgetClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
widgetVExpandSetPassive w = passivePropertyNE w widgetVExpandSet


-- @A: widgetVisible
widgetVisiblePassive :: WidgetClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
widgetVisiblePassive w = passivePropertyNE w widgetVisible


-- @A: widgetWidthRequest
widgetWidthRequestPassive :: WidgetClass self => (self) -> ReactiveFieldReadWrite IO (Int)
widgetWidthRequestPassive w = passivePropertyNE w widgetWidthRequest


-- @S: windowStateEvent
-- TODO
-- @S: actionActivated
actionActivatedReactive :: ActionClass self => self -> ReactiveFieldRead IO ()
actionActivatedReactive = (`reactiveSignalIO` actionActivated)


-- @A: actionAlwaysShowImage
actionAlwaysShowImagePassive :: ActionClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
actionAlwaysShowImagePassive w = passivePropertyNE w actionAlwaysShowImage


-- @G: actionGetAccelPath					 | ["action","Get","Accel","Path"]
actionGetAccelPathPassive :: (ActionClass self, GlibString string) => (self) -> ReactiveFieldRead IO ((Maybe string))
actionGetAccelPathPassive w = wrapMRPassive (actionGetAccelPath w)


-- @G: actionGetName					 | ["action","Get","Name"]
actionGetNamePassive :: (ActionClass self, GlibString string) => (self) -> ReactiveFieldRead IO (string)
actionGetNamePassive w = wrapMRPassive (actionGetName w)


-- @G: actionGetProxies					 | ["action","Get","Proxies"]
actionGetProxiesPassive :: ActionClass self => (self) -> ReactiveFieldRead IO ([Widget])
actionGetProxiesPassive w = wrapMRPassive (actionGetProxies w)


-- @G: actionGetSensitive					 | ["action","Get","Sensitive"]
actionGetSensitivePassive :: ActionClass self => (self) -> ReactiveFieldRead IO (Bool)
actionGetSensitivePassive w = wrapMRPassive (actionGetSensitive w)


-- @G: actionGetVisible					 | ["action","Get","Visible"]
actionGetVisiblePassive :: ActionClass self => (self) -> ReactiveFieldRead IO (Bool)
actionGetVisiblePassive w = wrapMRPassive (actionGetVisible w)


-- @A: actionHideIfEmpty
actionHideIfEmptyPassive :: ActionClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
actionHideIfEmptyPassive w = passivePropertyNE w actionHideIfEmpty


-- @A: actionIsImportant
actionIsImportantPassive :: ActionClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
actionIsImportantPassive w = passivePropertyNE w actionIsImportant


-- @A: actionLabel
actionLabelPassive :: (GlibString string, ActionClass self) => (self) -> ReactiveFieldReadWrite IO (string)
actionLabelPassive w = passivePropertyNE w actionLabel


-- @A: actionName
actionNamePassive :: (GlibString string, ActionClass self) => (self) -> ReactiveFieldReadWrite IO (string)
actionNamePassive w = passivePropertyNE w actionName


-- @A: actionSensitive
actionSensitivePassive :: ActionClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
actionSensitivePassive w = passivePropertyNE w actionSensitive


-- @T: actionSetAccelGroup					 | ["action","Set","Accel","Group"]

actionSetAccelGroupReactive w = wrapMW (actionSetAccelGroup w)


-- @T: actionSetAccelPath					 | ["action","Set","Accel","Path"]
actionSetAccelPathReactive :: (ActionClass self, GlibString string) => (self) -> ReactiveFieldWrite IO (string)
actionSetAccelPathReactive w = wrapMW (actionSetAccelPath w)


-- @T: actionSetSensitive					 | ["action","Set","Sensitive"]
actionSetSensitiveReactive :: ActionClass self => (self) -> ReactiveFieldWrite IO (Bool)
actionSetSensitiveReactive w = wrapMW (actionSetSensitive w)


-- @T: actionSetVisible					 | ["action","Set","Visible"]
actionSetVisibleReactive :: ActionClass self => (self) -> ReactiveFieldWrite IO (Bool)
actionSetVisibleReactive w = wrapMW (actionSetVisible w)


-- @A: actionShortLabel
actionShortLabelPassive :: (GlibString string, ActionClass self) => (self) -> ReactiveFieldReadWrite IO (string)
actionShortLabelPassive w = passivePropertyNE w actionShortLabel


-- @A: actionStockId
actionStockIdPassive :: (GlibString string, ActionClass self) => (self) -> ReactiveFieldReadWrite IO ((Maybe string))
actionStockIdPassive w = passivePropertyNE w actionStockId


-- @A: actionTooltip
actionTooltipPassive :: (GlibString string, ActionClass self) => (self) -> ReactiveFieldReadWrite IO ((Maybe string))
actionTooltipPassive w = passivePropertyNE w actionTooltip


-- @A: actionVisible
actionVisiblePassive :: ActionClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
actionVisiblePassive w = passivePropertyNE w actionVisible


-- @A: actionVisibleHorizontal
actionVisibleHorizontalPassive :: ActionClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
actionVisibleHorizontalPassive w = passivePropertyNE w actionVisibleHorizontal


-- @A: actionVisibleOverflown
actionVisibleOverflownPassive :: ActionClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
actionVisibleOverflownPassive w = passivePropertyNE w actionVisibleOverflown


-- @A: actionVisibleVertical
actionVisibleVerticalPassive :: ActionClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
actionVisibleVerticalPassive w = passivePropertyNE w actionVisibleVertical


-- @C: afterActionActivate
afterActionActivateReactive :: ActionClass self => self -> ReactiveFieldRead IO ()
afterActionActivateReactive w = reactivePropertyH_ w afterActionActivate

-- @G: actionGroupGetAction					 | ["action","Group","Get","Action"]
-- TODO
-- @G: actionGroupGetName					 | ["action","Group","Get","Name"]
actionGroupGetNamePassive :: GlibString string => (ActionGroup) -> ReactiveFieldRead IO (string)
actionGroupGetNamePassive w = wrapMRPassive (actionGroupGetName w)


-- @G: actionGroupGetSensitive					 | ["action","Group","Get","Sensitive"]
actionGroupGetSensitivePassive :: (ActionGroup) -> ReactiveFieldRead IO (Bool)
actionGroupGetSensitivePassive w = wrapMRPassive (actionGroupGetSensitive w)


-- @G: actionGroupGetVisible					 | ["action","Group","Get","Visible"]
actionGroupGetVisiblePassive :: (ActionGroup) -> ReactiveFieldRead IO (Bool)
actionGroupGetVisiblePassive w = wrapMRPassive (actionGroupGetVisible w)


-- @A: actionGroupName
actionGroupNamePassive :: GlibString string => ActionGroup -> ReactiveFieldReadWrite IO string
actionGroupNamePassive w = passivePropertyNE w actionGroupName


-- @A: actionGroupSensitive
actionGroupSensitivePassive :: ActionGroup -> ReactiveFieldReadWrite IO Bool
actionGroupSensitivePassive w = passivePropertyNE w actionGroupSensitive


-- @T: actionGroupSetSensitive					 | ["action","Group","Set","Sensitive"]
actionGroupSetSensitivePassive :: (ActionGroup) -> ReactiveFieldWrite IO (Bool)
actionGroupSetSensitivePassive w = wrapMW (actionGroupSetSensitive w)

-- @T: actionGroupSetTranslateFunc					 | ["action","Group","Set","Translate","Func"]
actionGroupSetTranslateFuncReactive :: GlibString string => (ActionGroup) -> ReactiveFieldWrite IO ((string -> IO string))
actionGroupSetTranslateFuncReactive w = wrapMW (actionGroupSetTranslateFunc w)


-- @T: actionGroupSetTranslationDomain					 | ["action","Group","Set","Translation","Domain"]
actionGroupSetTranslationDomainReactive :: GlibString string => (ActionGroup) -> ReactiveFieldWrite IO (string)
actionGroupSetTranslationDomainReactive w = wrapMW (actionGroupSetTranslationDomain w)


-- @T: actionGroupSetVisible					 | ["action","Group","Set","Visible"]
actionGroupSetVisiblePassive :: (ActionGroup) -> ReactiveFieldWrite IO (Bool)
actionGroupSetVisiblePassive w = wrapMW (actionGroupSetVisible w)

-- @A: actionGroupVisible
actionGroupVisiblePassive :: ActionGroup -> ReactiveFieldReadWrite IO Bool
actionGroupVisiblePassive w = passivePropertyNE w actionGroupVisible


-- @C: onActionActivate
onActionActivateReactive :: ActionClass self => self -> ReactiveFieldRead IO ()
onActionActivateReactive w = reactivePropertyH_ w onActionActivate

-- @C: afterRadioActionChanged
-- TODO
-- @C: onRadioActionChanged
-- TODO
-- @S: radioActionChanged
-- TODO
-- @A: radioActionCurrentValue
radioActionCurrentValuePassive :: RadioActionClass self => (self) -> ReactiveFieldReadWrite IO (Int)
radioActionCurrentValuePassive w = passivePropertyNE w radioActionCurrentValue


-- @G: radioActionGetCurrentValue					 | ["radio","Action","Get","Current","Value"]
radioActionGetCurrentValuePassive :: RadioActionClass self => (self) -> ReactiveFieldRead IO (Int)
radioActionGetCurrentValuePassive w = wrapMRPassive (radioActionGetCurrentValue w)


-- @G: radioActionGetGroup					 | ["radio","Action","Get","Group"]
radioActionGetGroupPassive :: RadioActionClass self => (self) -> ReactiveFieldRead IO ([RadioAction])
radioActionGetGroupPassive w = wrapMRPassive (radioActionGetGroup w)


-- @T: radioActionSetGroup					 | ["radio","Action","Set","Group"]
radioActionSetGroupReactive :: (RadioActionClass self, RadioActionClass groupMember) => (self) -> ReactiveFieldWrite IO (groupMember)
radioActionSetGroupReactive w = wrapMW (radioActionSetGroup w)


-- @A: radioActionValueAttr
radioActionValueAttrPassive :: RadioActionClass self => (self) -> ReactiveFieldReadWrite IO (Int)
radioActionValueAttrPassive w = passivePropertyNE w radioActionValueAttr


-- @A: recentActionShowNumbers
recentActionShowNumbersPassive :: RecentActionClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
recentActionShowNumbersPassive w = passivePropertyNE w recentActionShowNumbers


-- @S: actionToggled
actionToggledReactive :: ToggleActionClass self => self -> ReactiveFieldRead IO ()
actionToggledReactive = (`reactiveSignalIO` actionToggled)


-- @C: afterActionToggled
afterActionToggledReactive :: ToggleActionClass self => self -> ReactiveFieldRead IO ()
afterActionToggledReactive w = reactivePropertyH_ w afterActionToggled

-- @C: onActionToggled
onActionToggledReactive :: ToggleActionClass self => self -> ReactiveFieldRead IO ()
onActionToggledReactive w = reactivePropertyH_ w onActionToggled

-- @A: toggleActionActive
toggleActionActivePassive :: ToggleActionClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
toggleActionActivePassive w = passivePropertyNE w toggleActionActive


-- @A: toggleActionDrawAsRadio
toggleActionDrawAsRadioPassive :: ToggleActionClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
toggleActionDrawAsRadioPassive w = passivePropertyNE w toggleActionDrawAsRadio


-- @G: toggleActionGetActive					 | ["toggle","Action","Get","Active"]
toggleActionGetActivePassive :: ToggleActionClass self => (self) -> ReactiveFieldRead IO (Bool)
toggleActionGetActivePassive w = wrapMRPassive (toggleActionGetActive w)


-- @G: toggleActionGetDrawAsRadio					 | ["toggle","Action","Get","Draw","As","Radio"]
toggleActionGetDrawAsRadioPassive :: ToggleActionClass self => (self) -> ReactiveFieldRead IO (Bool)
toggleActionGetDrawAsRadioPassive w = wrapMRPassive (toggleActionGetDrawAsRadio w)


-- @T: toggleActionSetActive					 | ["toggle","Action","Set","Active"]
toggleActionSetActiveReactive :: ToggleActionClass self => (self) -> ReactiveFieldWrite IO (Bool)
toggleActionSetActiveReactive w = wrapMW (toggleActionSetActive w)


-- @T: toggleActionSetDrawAsRadio					 | ["toggle","Action","Set","Draw","As","Radio"]
toggleActionSetDrawAsRadioReactive :: ToggleActionClass self => (self) -> ReactiveFieldWrite IO (Bool)
toggleActionSetDrawAsRadioReactive w = wrapMW (toggleActionSetDrawAsRadio w)


-- @S: actionsChanged
actionsChangedReactive :: UIManagerClass self => self -> ReactiveFieldRead IO ()
actionsChangedReactive = (`reactiveSignalIO` actionsChanged)


-- @S: addWidget
-- TODO
-- @C: afterActionsChanged
afterActionsChangedReactive :: UIManagerClass self => self -> ReactiveFieldRead IO ()
afterActionsChangedReactive w = reactivePropertyH_ w afterActionsChanged

-- @C: afterAddWidget
-- TODO
-- @C: afterConnectProxy
-- TODO
-- @C: afterDisconnectProxy
-- TODO
-- @C: afterPostActivate
-- TODO
-- @C: afterPreActivate
-- TODO
-- @C: onActionsChanged
onActionsChangedReactive :: UIManagerClass self => self -> ReactiveFieldRead IO ()
onActionsChangedReactive w = reactivePropertyH_ w onActionsChanged

-- @C: onAddWidget
-- TODO
-- @C: onConnectProxy
-- TODO
-- @C: onDisconnectProxy
-- TODO
-- @C: onPostActivate
-- TODO
-- @C: onPreActivate
-- TODO
-- @A: uiManagerAddTearoffs
uiManagerAddTearoffsPassive :: UIManager -> ReactiveFieldReadWrite IO Bool
uiManagerAddTearoffsPassive w = passivePropertyNE w uiManagerAddTearoffs


-- @G: uiManagerGetAccelGroup					 | ["ui","Manager","Get","Accel","Group"]

uiManagerGetAccelGroupPassive w = wrapMRPassive (uiManagerGetAccelGroup w)


-- @G: uiManagerGetActionGroups					 | ["ui","Manager","Get","Action","Groups"]
uiManagerGetActionGroupsPassive :: (UIManager) -> ReactiveFieldRead IO ([ActionGroup])
uiManagerGetActionGroupsPassive w = wrapMRPassive (uiManagerGetActionGroups w)


-- @G: uiManagerGetAction					 | ["ui","Manager","Get","Action"]
-- TODO
-- @G: uiManagerGetAddTearoffs					 | ["ui","Manager","Get","Add","Tearoffs"]
uiManagerGetAddTearoffsPassive :: (UIManager) -> ReactiveFieldRead IO (Bool)
uiManagerGetAddTearoffsPassive w = wrapMRPassive (uiManagerGetAddTearoffs w)


-- @G: uiManagerGetToplevels					 | ["ui","Manager","Get","Toplevels"]
-- TODO
-- @G: uiManagerGetUi					 | ["ui","Manager","Get","Ui"]
uiManagerGetUiPassive :: GlibString string => (UIManager) -> ReactiveFieldRead IO (string)
uiManagerGetUiPassive w = wrapMRPassive (uiManagerGetUi w)


-- @G: uiManagerGetWidget					 | ["ui","Manager","Get","Widget"]
-- TODO
-- @T: uiManagerSetAddTearoffs					 | ["ui","Manager","Set","Add","Tearoffs"]
uiManagerSetAddTearoffsPassive :: (UIManager) -> ReactiveFieldWrite IO (Bool)
uiManagerSetAddTearoffsPassive w = wrapMW (uiManagerSetAddTearoffs w)

-- @G: builderGetObjectRaw					 | ["builder","Get","Object","Raw"]
-- TODO
-- @G: builderGetObjects					 | ["builder","Get","Objects"]
builderGetObjectsPassive :: (Builder) -> ReactiveFieldRead IO ([GObject])
builderGetObjectsPassive w = wrapMRPassive (builderGetObjects w)


-- @G: builderGetObject					 | ["builder","Get","Object"]
-- TODO
-- @G: builderGetTranslationDomain					 | ["builder","Get","Translation","Domain"]
builderGetTranslationDomainPassive :: GlibString string => (Builder) -> ReactiveFieldRead IO ((Maybe string))
builderGetTranslationDomainPassive w = wrapMRPassive (builderGetTranslationDomain w)


-- @T: builderSetTranslationDomain					 | ["builder","Set","Translation","Domain"]
builderSetTranslationDomainReactive :: GlibString string => (Builder) -> ReactiveFieldWrite IO (Maybe string)
builderSetTranslationDomainReactive w = wrapMW (builderSetTranslationDomain w)


-- @C: afterButtonActivate
afterButtonActivateReactive :: ButtonClass self => self -> ReactiveFieldRead IO ()
afterButtonActivateReactive w = reactivePropertyH_ w afterButtonActivate

-- @C: afterClicked
afterClickedReactive :: ButtonClass self => self -> ReactiveFieldRead IO ()
afterClickedReactive w = reactivePropertyH_ w afterClicked

-- @C: afterEnter
afterEnterReactive :: ButtonClass self => self -> ReactiveFieldRead IO ()
afterEnterReactive w = reactivePropertyH_ w afterEnter

-- @C: afterLeave
afterLeaveReactive :: ButtonClass self => self -> ReactiveFieldRead IO ()
afterLeaveReactive w = reactivePropertyH_ w afterLeave

-- @C: afterPressed
afterPressedReactive :: ButtonClass self => self -> ReactiveFieldRead IO ()
afterPressedReactive w = reactivePropertyH_ w afterPressed

-- @C: afterReleased
afterReleasedReactive :: ButtonClass self => self -> ReactiveFieldRead IO ()
afterReleasedReactive w = reactivePropertyH_ w afterReleased

-- @S: buttonActivated
buttonActivatedReactive :: ButtonClass self => self -> ReactiveFieldRead IO ()
buttonActivatedReactive = (`reactiveSignalIO` buttonActivated)


-- @A: buttonFocusOnClick
buttonFocusOnClickPassive :: ButtonClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
buttonFocusOnClickPassive w = passivePropertyNE w buttonFocusOnClick


-- @G: buttonGetAlignment					 | ["button","Get","Alignment"]
buttonGetAlignmentPassive :: ButtonClass self => (self) -> ReactiveFieldRead IO ((Float, Float))
buttonGetAlignmentPassive w = wrapMRPassive (buttonGetAlignment w)


-- @G: buttonGetEventWindow					 | ["button","Get","Event","Window"]
buttonGetEventWindowPassive :: ButtonClass self => (self) -> ReactiveFieldRead IO ((Maybe DrawWindow))
buttonGetEventWindowPassive w = wrapMRPassive (buttonGetEventWindow w)


-- @G: buttonGetFocusOnClick					 | ["button","Get","Focus","On","Click"]
buttonGetFocusOnClickPassive :: ButtonClass self => (self) -> ReactiveFieldRead IO (Bool)
buttonGetFocusOnClickPassive w = wrapMRPassive (buttonGetFocusOnClick w)


-- @G: buttonGetImage					 | ["button","Get","Image"]
buttonGetImagePassive :: ButtonClass self => (self) -> ReactiveFieldRead IO ((Maybe Widget))
buttonGetImagePassive w = wrapMRPassive (buttonGetImage w)


-- @G: buttonGetImagePosition					 | ["button","Get","Image","Position"]
buttonGetImagePositionPassive :: ButtonClass self => (self) -> ReactiveFieldRead IO (PositionType)
buttonGetImagePositionPassive w = wrapMRPassive (buttonGetImagePosition w)


-- @G: buttonGetLabel					 | ["button","Get","Label"]
buttonGetLabelPassive :: (ButtonClass self, GlibString string) => (self) -> ReactiveFieldRead IO (string)
buttonGetLabelPassive w = wrapMRPassive (buttonGetLabel w)


-- @G: buttonGetRelief					 | ["button","Get","Relief"]
buttonGetReliefPassive :: ButtonClass self => (self) -> ReactiveFieldRead IO (ReliefStyle)
buttonGetReliefPassive w = wrapMRPassive (buttonGetRelief w)


-- @G: buttonGetUseStock					 | ["button","Get","Use","Stock"]
buttonGetUseStockPassive :: ButtonClass self => (self) -> ReactiveFieldRead IO (Bool)
buttonGetUseStockPassive w = wrapMRPassive (buttonGetUseStock w)


-- @G: buttonGetUseUnderline					 | ["button","Get","Use","Underline"]
buttonGetUseUnderlinePassive :: ButtonClass self => (self) -> ReactiveFieldRead IO (Bool)
buttonGetUseUnderlinePassive w = wrapMRPassive (buttonGetUseUnderline w)


-- @A: buttonImagePosition
buttonImagePositionPassive :: ButtonClass self => (self) -> ReactiveFieldReadWrite IO (PositionType)
buttonImagePositionPassive w = passivePropertyNE w buttonImagePosition


-- @A: buttonLabel
buttonLabelPassive :: (ButtonClass self, GlibString string) => (self) -> ReactiveFieldReadWrite IO (string)
buttonLabelPassive w = passivePropertyNE w buttonLabel


-- @A: buttonRelief
buttonReliefPassive :: ButtonClass self => (self) -> ReactiveFieldReadWrite IO (ReliefStyle)
buttonReliefPassive w = passivePropertyNE w buttonRelief


-- @T: buttonSetAlignment					 | ["button","Set","Alignment"]
buttonSetAlignmentReactive :: ButtonClass self => (self) -> ReactiveFieldWrite IO ((Float, Float))
buttonSetAlignmentReactive w = wrapMW (buttonSetAlignment w)


-- @T: buttonSetFocusOnClick					 | ["button","Set","Focus","On","Click"]
buttonSetFocusOnClickReactive :: ButtonClass self => (self) -> ReactiveFieldWrite IO (Bool)
buttonSetFocusOnClickReactive w = wrapMW (buttonSetFocusOnClick w)


-- @T: buttonSetImage					 | ["button","Set","Image"]
buttonSetImageReactive :: (ButtonClass self, WidgetClass image) => (self) -> ReactiveFieldWrite IO (image)
buttonSetImageReactive w = wrapMW (buttonSetImage w)


-- @T: buttonSetImagePosition					 | ["button","Set","Image","Position"]
buttonSetImagePositionReactive :: ButtonClass self => (self) -> ReactiveFieldWrite IO (PositionType)
buttonSetImagePositionReactive w = wrapMW (buttonSetImagePosition w)


-- @T: buttonSetLabel					 | ["button","Set","Label"]
buttonSetLabelReactive :: (ButtonClass self, GlibString string) => (self) -> ReactiveFieldWrite IO (string)
buttonSetLabelReactive w = wrapMW (buttonSetLabel w)


-- @T: buttonSetRelief					 | ["button","Set","Relief"]
buttonSetReliefReactive :: ButtonClass self => (self) -> ReactiveFieldWrite IO (ReliefStyle)
buttonSetReliefReactive w = wrapMW (buttonSetRelief w)


-- @T: buttonSetUseStock					 | ["button","Set","Use","Stock"]
buttonSetUseStockReactive :: ButtonClass self => (self) -> ReactiveFieldWrite IO (Bool)
buttonSetUseStockReactive w = wrapMW (buttonSetUseStock w)


-- @T: buttonSetUseUnderline					 | ["button","Set","Use","Underline"]
buttonSetUseUnderlineReactive :: ButtonClass self => (self) -> ReactiveFieldWrite IO (Bool)
buttonSetUseUnderlineReactive w = wrapMW (buttonSetUseUnderline w)


-- @A: buttonUseStock
buttonUseStockPassive :: ButtonClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
buttonUseStockPassive w = passivePropertyNE w buttonUseStock


-- @A: buttonUseUnderline
buttonUseUnderlinePassive :: ButtonClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
buttonUseUnderlinePassive w = passivePropertyNE w buttonUseUnderline


-- @A: buttonXalign
buttonXalignPassive :: ButtonClass self => (self) -> ReactiveFieldReadWrite IO (Float)
buttonXalignPassive w = passivePropertyNE w buttonXalign


-- @A: buttonYalign
buttonYalignPassive :: ButtonClass self => (self) -> ReactiveFieldReadWrite IO (Float)
buttonYalignPassive w = passivePropertyNE w buttonYalign


-- @C: onButtonActivate
onButtonActivateReactive :: ButtonClass self => self -> ReactiveFieldRead IO ()
onButtonActivateReactive w = reactivePropertyH_ w onButtonActivate

-- @C: onClicked
onClickedReactive :: ButtonClass self => self -> ReactiveFieldRead IO ()
onClickedReactive w = reactivePropertyH_ w onClicked

-- @C: onEnter
onEnterReactive :: ButtonClass self => self -> ReactiveFieldRead IO ()
onEnterReactive w = reactivePropertyH_ w onEnter

-- @C: onLeave
onLeaveReactive :: ButtonClass self => self -> ReactiveFieldRead IO ()
onLeaveReactive w = reactivePropertyH_ w onLeave

-- @C: onPressed
onPressedReactive :: ButtonClass self => self -> ReactiveFieldRead IO ()
onPressedReactive w = reactivePropertyH_ w onPressed

-- @C: onReleased
onReleasedReactive :: ButtonClass self => self -> ReactiveFieldRead IO ()
onReleasedReactive w = reactivePropertyH_ w onReleased

-- @T: linkButtonSetUriHook					 | ["link","Button","Set","Uri","Hook"]
-- TODO
-- @A: linkButtonURI
linkButtonURIPassive :: (LinkButtonClass self, GlibString string) => (self) -> ReactiveFieldReadWrite IO (string)
linkButtonURIPassive w = passivePropertyNE w linkButtonURI


-- @A: linkButtonVisited
linkButtonVisitedPassive :: LinkButtonClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
linkButtonVisitedPassive w = passivePropertyNE w linkButtonVisited


-- @C: afterGroupChanged
afterGroupChangedReactive :: RadioButtonClass self => self -> ReactiveFieldRead IO ()
afterGroupChangedReactive w = reactivePropertyH_ w afterGroupChanged

-- @S: groupChanged
groupChangedReactive :: RadioButtonClass self => self -> ReactiveFieldRead IO ()
groupChangedReactive = (`reactiveSignalIO` groupChanged)


-- @C: onGroupChanged
onGroupChangedReactive :: RadioButtonClass self => self -> ReactiveFieldRead IO ()
onGroupChangedReactive w = reactivePropertyH_ w onGroupChanged

-- @G: radioButtonGetGroup					 | ["radio","Button","Get","Group"]
radioButtonGetGroupPassive :: (RadioButton) -> ReactiveFieldRead IO ([RadioButton])
radioButtonGetGroupPassive w = wrapMRPassive (radioButtonGetGroup w)


-- @T: radioButtonSetGroup					 | ["radio","Button","Set","Group"]
radioButtonSetGroupPassive :: (RadioButton) -> ReactiveFieldWrite IO (RadioButton)
radioButtonSetGroupPassive w = wrapMW (radioButtonSetGroup w)

-- @A: scaleButtonAdjustment
scaleButtonAdjustmentPassive :: ScaleButtonClass self => (self) -> ReactiveFieldReadWrite IO (Adjustment)
scaleButtonAdjustmentPassive w = passivePropertyNE w scaleButtonAdjustment


-- @G: scaleButtonGetMinusButton					 | ["scale","Button","Get","Minus","Button"]
scaleButtonGetMinusButtonPassive :: ScaleButtonClass self => (self) -> ReactiveFieldRead IO (Widget)
scaleButtonGetMinusButtonPassive w = wrapMRPassive (scaleButtonGetMinusButton w)


-- @G: scaleButtonGetPlusButton					 | ["scale","Button","Get","Plus","Button"]
scaleButtonGetPlusButtonPassive :: ScaleButtonClass self => (self) -> ReactiveFieldRead IO (Widget)
scaleButtonGetPlusButtonPassive w = wrapMRPassive (scaleButtonGetPlusButton w)


-- @G: scaleButtonGetPopup					 | ["scale","Button","Get","Popup"]
scaleButtonGetPopupPassive :: ScaleButtonClass self => (self) -> ReactiveFieldRead IO (Widget)
scaleButtonGetPopupPassive w = wrapMRPassive (scaleButtonGetPopup w)


-- @S: scaleButtonPopdown
scaleButtonPopdownReactive :: ScaleButtonClass self => self -> ReactiveFieldRead IO ()
scaleButtonPopdownReactive = (`reactiveSignalIO` scaleButtonPopdown)


-- @S: scaleButtonPopup
scaleButtonPopupReactive :: ScaleButtonClass self => self -> ReactiveFieldRead IO ()
scaleButtonPopupReactive = (`reactiveSignalIO` scaleButtonPopup)


-- @T: scaleButtonSetIcons					 | ["scale","Button","Set","Icons"]
scaleButtonSetIconsReactive :: (ScaleButtonClass self, GlibString string) => (self) -> ReactiveFieldWrite IO ([string])
scaleButtonSetIconsReactive w = wrapMW (scaleButtonSetIcons w)


-- @A: scaleButtonSize
scaleButtonSizePassive :: ScaleButtonClass self => (self) -> ReactiveFieldReadWrite IO (IconSize)
scaleButtonSizePassive w = passivePropertyNE w scaleButtonSize


-- @S: scaleButtonValueChanged
-- TODO
-- @A: scaleButtonValue
scaleButtonValuePassive :: ScaleButtonClass self => (self) -> ReactiveFieldReadWrite IO (Double)
scaleButtonValuePassive w = passivePropertyNE w scaleButtonValue


-- @C: afterToggled
afterToggledReactive :: ToggleButtonClass self => self -> ReactiveFieldRead IO ()
afterToggledReactive w = reactivePropertyH_ w afterToggled

-- @C: onToggled
onToggledReactive :: ToggleButtonClass self => self -> ReactiveFieldRead IO ()
onToggledReactive w = reactivePropertyH_ w onToggled

-- @A: toggleButtonActive
toggleButtonActivePassive :: ToggleButtonClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
toggleButtonActivePassive w = passivePropertyNE w toggleButtonActive


-- @A: toggleButtonDrawIndicator
toggleButtonDrawIndicatorPassive :: ToggleButtonClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
toggleButtonDrawIndicatorPassive w = passivePropertyNE w toggleButtonDrawIndicator


-- @G: toggleButtonGetActive					 | ["toggle","Button","Get","Active"]
toggleButtonGetActivePassive :: ToggleButtonClass self => (self) -> ReactiveFieldRead IO (Bool)
toggleButtonGetActivePassive w = wrapMRPassive (toggleButtonGetActive w)


-- @G: toggleButtonGetInconsistent					 | ["toggle","Button","Get","Inconsistent"]
toggleButtonGetInconsistentPassive :: ToggleButtonClass self => (self) -> ReactiveFieldRead IO (Bool)
toggleButtonGetInconsistentPassive w = wrapMRPassive (toggleButtonGetInconsistent w)


-- @G: toggleButtonGetMode					 | ["toggle","Button","Get","Mode"]
toggleButtonGetModePassive :: ToggleButtonClass self => (self) -> ReactiveFieldRead IO (Bool)
toggleButtonGetModePassive w = wrapMRPassive (toggleButtonGetMode w)


-- @A: toggleButtonInconsistent
toggleButtonInconsistentPassive :: ToggleButtonClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
toggleButtonInconsistentPassive w = passivePropertyNE w toggleButtonInconsistent


-- @A: toggleButtonMode
toggleButtonModePassive :: ToggleButtonClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
toggleButtonModePassive w = passivePropertyNE w toggleButtonMode


-- @T: toggleButtonSetActive					 | ["toggle","Button","Set","Active"]
toggleButtonSetActiveReactive :: ToggleButtonClass self => (self) -> ReactiveFieldWrite IO (Bool)
toggleButtonSetActiveReactive w = wrapMW (toggleButtonSetActive w)


-- @T: toggleButtonSetInconsistent					 | ["toggle","Button","Set","Inconsistent"]
toggleButtonSetInconsistentReactive :: ToggleButtonClass self => (self) -> ReactiveFieldWrite IO (Bool)
toggleButtonSetInconsistentReactive w = wrapMW (toggleButtonSetInconsistent w)


-- @T: toggleButtonSetMode					 | ["toggle","Button","Set","Mode"]
toggleButtonSetModeReactive :: ToggleButtonClass self => (self) -> ReactiveFieldWrite IO (Bool)
toggleButtonSetModeReactive w = wrapMW (toggleButtonSetMode w)


-- @S: toggled
toggledReactive :: ToggleButtonClass self => self -> ReactiveFieldRead IO ()
toggledReactive = (`reactiveSignalIO` toggled)


-- @T: setSourcePixbuf					 | ["set","Source","Pixbuf"]
-- TODO
-- @G: accelLabelGetAccelWidget					 | ["accel","Label","Get","Accel","Widget"]
accelLabelGetAccelWidgetPassive :: AccelLabelClass self => (self) -> ReactiveFieldRead IO ((Maybe Widget))
accelLabelGetAccelWidgetPassive w = wrapMRPassive (accelLabelGetAccelWidget w)


-- @T: accelLabelSetAccelWidget					 | ["accel","Label","Set","Accel","Widget"]
accelLabelSetAccelWidgetReactive :: (AccelLabelClass self, WidgetClass accelWidget) => (self) -> ReactiveFieldWrite IO (accelWidget)
accelLabelSetAccelWidgetReactive w = wrapMW (accelLabelSetAccelWidget w)


-- @A: imageFile
imageFilePassive :: GlibString string => Image -> ReactiveFieldReadWrite IO string
imageFilePassive w = passivePropertyNE w imageFile


-- @G: imageGetPixbuf					 | ["image","Get","Pixbuf"]
imageGetPixbufPassive :: (Image) -> ReactiveFieldRead IO (Pixbuf)
imageGetPixbufPassive w = wrapMRPassive (imageGetPixbuf w)


-- @G: imageGetPixelSize					 | ["image","Get","Pixel","Size"]
imageGetPixelSizePassive :: (Image) -> ReactiveFieldRead IO (Int)
imageGetPixelSizePassive w = wrapMRPassive (imageGetPixelSize w)


-- @A: imageIconName
imageIconNamePassive :: GlibString string => Image -> ReactiveFieldReadWrite IO string
imageIconNamePassive w = passivePropertyNE w imageIconName


-- @A: imageIconSize
imageIconSizePassive :: Image -> ReactiveFieldReadWrite IO Int
imageIconSizePassive w = passivePropertyNE w imageIconSize


-- @A: imagePixelSize
imagePixelSizePassive :: Image -> ReactiveFieldReadWrite IO Int
imagePixelSizePassive w = passivePropertyNE w imagePixelSize


-- @T: imageSetFromAnimation					 | ["image","Set","From","Animation"]

imageSetFromAnimationReactive w = wrapMW (imageSetFromAnimation w)


-- @T: imageSetFromFile					 | ["image","Set","From","File"]
imageSetFromFileReactive :: GlibFilePath fp => (Image) -> ReactiveFieldWrite IO (fp)
imageSetFromFileReactive w = wrapMW (imageSetFromFile w)


-- @T: imageSetFromIconName					 | ["image","Set","From","Icon","Name"]
-- TODO
-- @T: imageSetFromPixbuf					 | ["image","Set","From","Pixbuf"]
imageSetFromPixbufPassive :: (Image) -> ReactiveFieldWrite IO (Pixbuf)
imageSetFromPixbufPassive w = wrapMW (imageSetFromPixbuf w)

-- @T: imageSetFromStock					 | ["image","Set","From","Stock"]
-- TODO
-- @T: imageSetPixelSize					 | ["image","Set","Pixel","Size"]
imageSetPixelSizePassive :: (Image) -> ReactiveFieldWrite IO (Int)
imageSetPixelSizePassive w = wrapMW (imageSetPixelSize w)

-- @A: imageStock
imageStockPassive :: GlibString string => Image -> ReactiveFieldReadWrite IO string
imageStockPassive w = passivePropertyNE w imageStock


-- @S: infoBarClose
infoBarCloseReactive :: InfoBarClass self => self -> ReactiveFieldRead IO ()
infoBarCloseReactive = (`reactiveSignalIO` infoBarClose)


-- @G: infoBarGetActionArea					 | ["info","Bar","Get","Action","Area"]
infoBarGetActionAreaPassive :: InfoBarClass self => (self) -> ReactiveFieldRead IO (Widget)
infoBarGetActionAreaPassive w = wrapMRPassive (infoBarGetActionArea w)


-- @G: infoBarGetContentArea					 | ["info","Bar","Get","Content","Area"]
infoBarGetContentAreaPassive :: InfoBarClass self => (self) -> ReactiveFieldRead IO (Widget)
infoBarGetContentAreaPassive w = wrapMRPassive (infoBarGetContentArea w)


-- @A: infoBarMessageType
infoBarMessageTypePassive :: InfoBarClass self => (self) -> ReactiveFieldReadWrite IO (MessageType)
infoBarMessageTypePassive w = passivePropertyNE w infoBarMessageType


-- @S: infoBarResponse
-- TODO
-- @T: infoBarSetDefaultResponse					 | ["info","Bar","Set","Default","Response"]
infoBarSetDefaultResponseReactive :: InfoBarClass self => (self) -> ReactiveFieldWrite IO (Int)
infoBarSetDefaultResponseReactive w = wrapMW (infoBarSetDefaultResponse w)


-- @T: infoBarSetResponseSensitive					 | ["info","Bar","Set","Response","Sensitive"]
-- TODO
-- @S: labelActiveCurrentLink
labelActiveCurrentLinkReactive :: LabelClass self => self -> ReactiveFieldRead IO ()
labelActiveCurrentLinkReactive = (`reactiveSignalIO` labelActiveCurrentLink)


-- @S: labelActiveLink
-- TODO
-- @A: labelAngle
labelAnglePassive :: LabelClass self => (self) -> ReactiveFieldReadWrite IO (Double)
labelAnglePassive w = passivePropertyNE w labelAngle


-- @A: labelAttributes
labelAttributesPassive :: LabelClass self => (self) -> ReactiveFieldReadWrite IO ([PangoAttribute])
labelAttributesPassive w = passivePropertyNE w labelAttributes


-- @S: labelCopyClipboard
labelCopyClipboardReactive :: LabelClass self => self -> ReactiveFieldRead IO ()
labelCopyClipboardReactive = (`reactiveSignalIO` labelCopyClipboard)


-- @A: labelEllipsize
labelEllipsizePassive :: LabelClass self => (self) -> ReactiveFieldReadWrite IO (EllipsizeMode)
labelEllipsizePassive w = passivePropertyNE w labelEllipsize


-- @G: labelGetAngle					 | ["label","Get","Angle"]
labelGetAnglePassive :: LabelClass self => (self) -> ReactiveFieldRead IO (Double)
labelGetAnglePassive w = wrapMRPassive (labelGetAngle w)


-- @G: labelGetAttributes					 | ["label","Get","Attributes"]
labelGetAttributesPassive :: LabelClass self => (self) -> ReactiveFieldRead IO ([PangoAttribute])
labelGetAttributesPassive w = wrapMRPassive (labelGetAttributes w)


-- @G: labelGetEllipsize					 | ["label","Get","Ellipsize"]
labelGetEllipsizePassive :: LabelClass self => (self) -> ReactiveFieldRead IO (EllipsizeMode)
labelGetEllipsizePassive w = wrapMRPassive (labelGetEllipsize w)


-- @G: labelGetJustify					 | ["label","Get","Justify"]
labelGetJustifyPassive :: LabelClass self => (self) -> ReactiveFieldRead IO (Justification)
labelGetJustifyPassive w = wrapMRPassive (labelGetJustify w)


-- @G: labelGetLabel					 | ["label","Get","Label"]
labelGetLabelPassive :: (LabelClass self, GlibString string) => (self) -> ReactiveFieldRead IO (string)
labelGetLabelPassive w = wrapMRPassive (labelGetLabel w)


-- @G: labelGetLayout					 | ["label","Get","Layout"]
labelGetLayoutPassive :: LabelClass self => (self) -> ReactiveFieldRead IO (PangoLayout)
labelGetLayoutPassive w = wrapMRPassive (labelGetLayout w)


-- @G: labelGetLayoutOffsets					 | ["label","Get","Layout","Offsets"]
labelGetLayoutOffsetsPassive :: LabelClass self => (self) -> ReactiveFieldRead IO ((Int, Int))
labelGetLayoutOffsetsPassive w = wrapMRPassive (labelGetLayoutOffsets w)


-- @G: labelGetLineWrap					 | ["label","Get","Line","Wrap"]
labelGetLineWrapPassive :: LabelClass self => (self) -> ReactiveFieldRead IO (Bool)
labelGetLineWrapPassive w = wrapMRPassive (labelGetLineWrap w)


-- @G: labelGetLineWrapMode					 | ["label","Get","Line","Wrap","Mode"]
labelGetLineWrapModePassive :: LabelClass self => (self) -> ReactiveFieldRead IO (LayoutWrapMode)
labelGetLineWrapModePassive w = wrapMRPassive (labelGetLineWrapMode w)


-- @G: labelGetMaxWidthChars					 | ["label","Get","Max","Width","Chars"]
labelGetMaxWidthCharsPassive :: LabelClass self => (self) -> ReactiveFieldRead IO (Int)
labelGetMaxWidthCharsPassive w = wrapMRPassive (labelGetMaxWidthChars w)


-- @G: labelGetMnemonicKeyval					 | ["label","Get","Mnemonic","Keyval"]
labelGetMnemonicKeyvalPassive :: LabelClass self => (self) -> ReactiveFieldRead IO (KeyVal)
labelGetMnemonicKeyvalPassive w = wrapMRPassive (labelGetMnemonicKeyval w)


-- @G: labelGetMnemonicWidget					 | ["label","Get","Mnemonic","Widget"]
labelGetMnemonicWidgetPassive :: LabelClass self => (self) -> ReactiveFieldRead IO ((Maybe Widget))
labelGetMnemonicWidgetPassive w = wrapMRPassive (labelGetMnemonicWidget w)


-- @G: labelGetSelectable					 | ["label","Get","Selectable"]
labelGetSelectablePassive :: LabelClass self => (self) -> ReactiveFieldRead IO (Bool)
labelGetSelectablePassive w = wrapMRPassive (labelGetSelectable w)


-- @G: labelGetSelectionBounds					 | ["label","Get","Selection","Bounds"]
labelGetSelectionBoundsPassive :: LabelClass self => (self) -> ReactiveFieldRead IO ((Maybe (Int, Int)))
labelGetSelectionBoundsPassive w = wrapMRPassive (labelGetSelectionBounds w)


-- @G: labelGetSingleLineMode					 | ["label","Get","Single","Line","Mode"]
labelGetSingleLineModePassive :: LabelClass self => (self) -> ReactiveFieldRead IO (Bool)
labelGetSingleLineModePassive w = wrapMRPassive (labelGetSingleLineMode w)


-- @G: labelGetText					 | ["label","Get","Text"]
labelGetTextPassive :: (LabelClass self, GlibString string) => (self) -> ReactiveFieldRead IO (string)
labelGetTextPassive w = wrapMRPassive (labelGetText w)


-- @G: labelGetUseMarkup					 | ["label","Get","Use","Markup"]
labelGetUseMarkupPassive :: LabelClass self => (self) -> ReactiveFieldRead IO (Bool)
labelGetUseMarkupPassive w = wrapMRPassive (labelGetUseMarkup w)


-- @G: labelGetUseUnderline					 | ["label","Get","Use","Underline"]
labelGetUseUnderlinePassive :: LabelClass self => (self) -> ReactiveFieldRead IO (Bool)
labelGetUseUnderlinePassive w = wrapMRPassive (labelGetUseUnderline w)


-- @G: labelGetWidthChars					 | ["label","Get","Width","Chars"]
labelGetWidthCharsPassive :: LabelClass self => (self) -> ReactiveFieldRead IO (Int)
labelGetWidthCharsPassive w = wrapMRPassive (labelGetWidthChars w)


-- @A: labelJustify
labelJustifyPassive :: LabelClass self => (self) -> ReactiveFieldReadWrite IO (Justification)
labelJustifyPassive w = passivePropertyNE w labelJustify


-- @A: labelLabel
labelLabelPassive :: (LabelClass self, GlibString string) => (self) -> ReactiveFieldReadWrite IO (string)
labelLabelPassive w = passivePropertyNE w labelLabel


-- @A: labelLineWrap
labelLineWrapPassive :: LabelClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
labelLineWrapPassive w = passivePropertyNE w labelLineWrap


-- @A: labelMaxWidthChars
labelMaxWidthCharsPassive :: LabelClass self => (self) -> ReactiveFieldReadWrite IO (Int)
labelMaxWidthCharsPassive w = passivePropertyNE w labelMaxWidthChars


-- @S: labelMoveCursor
-- TODO
-- @S: labelPopulatePopup
-- TODO
-- @A: labelSelectable
labelSelectablePassive :: LabelClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
labelSelectablePassive w = passivePropertyNE w labelSelectable


-- @T: labelSetAngle					 | ["label","Set","Angle"]
labelSetAngleReactive :: LabelClass self => (self) -> ReactiveFieldWrite IO (Double)
labelSetAngleReactive w = wrapMW (labelSetAngle w)


-- @T: labelSetAttributes					 | ["label","Set","Attributes"]
labelSetAttributesReactive :: LabelClass self => (self) -> ReactiveFieldWrite IO ([PangoAttribute])
labelSetAttributesReactive w = wrapMW (labelSetAttributes w)


-- @T: labelSetEllipsize					 | ["label","Set","Ellipsize"]
labelSetEllipsizeReactive :: LabelClass self => (self) -> ReactiveFieldWrite IO (EllipsizeMode)
labelSetEllipsizeReactive w = wrapMW (labelSetEllipsize w)


-- @T: labelSetJustify					 | ["label","Set","Justify"]
labelSetJustifyReactive :: LabelClass self => (self) -> ReactiveFieldWrite IO (Justification)
labelSetJustifyReactive w = wrapMW (labelSetJustify w)


-- @T: labelSetLabel					 | ["label","Set","Label"]
labelSetLabelReactive :: (LabelClass self, GlibString string) => (self) -> ReactiveFieldWrite IO (string)
labelSetLabelReactive w = wrapMW (labelSetLabel w)


-- @T: labelSetLineWrap					 | ["label","Set","Line","Wrap"]
labelSetLineWrapReactive :: LabelClass self => (self) -> ReactiveFieldWrite IO (Bool)
labelSetLineWrapReactive w = wrapMW (labelSetLineWrap w)


-- @T: labelSetLineWrapMode					 | ["label","Set","Line","Wrap","Mode"]
labelSetLineWrapModeReactive :: LabelClass self => (self) -> ReactiveFieldWrite IO (LayoutWrapMode)
labelSetLineWrapModeReactive w = wrapMW (labelSetLineWrapMode w)


-- @T: labelSetMarkup					 | ["label","Set","Markup"]
labelSetMarkupReactive :: (LabelClass self, GlibString markup) => (self) -> ReactiveFieldWrite IO (markup)
labelSetMarkupReactive w = wrapMW (labelSetMarkup w)


-- @T: labelSetMarkupWithMnemonic					 | ["label","Set","Markup","With","Mnemonic"]
labelSetMarkupWithMnemonicReactive :: (LabelClass self, GlibString markup) => (self) -> ReactiveFieldWrite IO (markup)
labelSetMarkupWithMnemonicReactive w = wrapMW (labelSetMarkupWithMnemonic w)


-- @T: labelSetMaxWidthChars					 | ["label","Set","Max","Width","Chars"]
labelSetMaxWidthCharsReactive :: LabelClass self => (self) -> ReactiveFieldWrite IO (Int)
labelSetMaxWidthCharsReactive w = wrapMW (labelSetMaxWidthChars w)


-- @T: labelSetMnemonicWidget					 | ["label","Set","Mnemonic","Widget"]
labelSetMnemonicWidgetReactive :: (LabelClass self, WidgetClass widget) => (self) -> ReactiveFieldWrite IO (widget)
labelSetMnemonicWidgetReactive w = wrapMW (labelSetMnemonicWidget w)


-- @T: labelSetPattern					 | ["label","Set","Pattern"]
labelSetPatternReactive :: LabelClass l => (l) -> ReactiveFieldWrite IO ([Int])
labelSetPatternReactive w = wrapMW (labelSetPattern w)


-- @T: labelSetSelectable					 | ["label","Set","Selectable"]
labelSetSelectableReactive :: LabelClass self => (self) -> ReactiveFieldWrite IO (Bool)
labelSetSelectableReactive w = wrapMW (labelSetSelectable w)


-- @T: labelSetSingleLineMode					 | ["label","Set","Single","Line","Mode"]
labelSetSingleLineModeReactive :: LabelClass self => (self) -> ReactiveFieldWrite IO (Bool)
labelSetSingleLineModeReactive w = wrapMW (labelSetSingleLineMode w)


-- @T: labelSetText					 | ["label","Set","Text"]
labelSetTextReactive :: (LabelClass self, GlibString string) => (self) -> ReactiveFieldWrite IO (string)
labelSetTextReactive w = wrapMW (labelSetText w)


-- @T: labelSetTextWithMnemonic					 | ["label","Set","Text","With","Mnemonic"]
labelSetTextWithMnemonicReactive :: (LabelClass self, GlibString string) => (self) -> ReactiveFieldWrite IO (string)
labelSetTextWithMnemonicReactive w = wrapMW (labelSetTextWithMnemonic w)


-- @T: labelSetUseMarkup					 | ["label","Set","Use","Markup"]
labelSetUseMarkupReactive :: LabelClass self => (self) -> ReactiveFieldWrite IO (Bool)
labelSetUseMarkupReactive w = wrapMW (labelSetUseMarkup w)


-- @T: labelSetUseUnderline					 | ["label","Set","Use","Underline"]
labelSetUseUnderlineReactive :: LabelClass self => (self) -> ReactiveFieldWrite IO (Bool)
labelSetUseUnderlineReactive w = wrapMW (labelSetUseUnderline w)


-- @T: labelSetWidthChars					 | ["label","Set","Width","Chars"]
labelSetWidthCharsReactive :: LabelClass self => (self) -> ReactiveFieldWrite IO (Int)
labelSetWidthCharsReactive w = wrapMW (labelSetWidthChars w)


-- @A: labelSingleLineMode
labelSingleLineModePassive :: LabelClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
labelSingleLineModePassive w = passivePropertyNE w labelSingleLineMode


-- @A: labelText
labelTextPassive :: (LabelClass self, GlibString string) => (self) -> ReactiveFieldReadWrite IO (string)
labelTextPassive w = passivePropertyNE w labelText


-- @A: labelUseMarkup
labelUseMarkupPassive :: LabelClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
labelUseMarkupPassive w = passivePropertyNE w labelUseMarkup


-- @A: labelUseUnderline
labelUseUnderlinePassive :: LabelClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
labelUseUnderlinePassive w = passivePropertyNE w labelUseUnderline


-- @A: labelWidthChars
labelWidthCharsPassive :: LabelClass self => (self) -> ReactiveFieldReadWrite IO (Int)
labelWidthCharsPassive w = passivePropertyNE w labelWidthChars


-- @A: labelWrap
labelWrapPassive :: LabelClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
labelWrapPassive w = passivePropertyNE w labelWrap


-- @A: labelWrapMode
labelWrapModePassive :: LabelClass self => (self) -> ReactiveFieldReadWrite IO (LayoutWrapMode)
labelWrapModePassive w = passivePropertyNE w labelWrapMode


-- @A: progressBarDiscreteBlocks
progressBarDiscreteBlocksPassive :: ProgressBarClass self => (self) -> ReactiveFieldReadWrite IO (Int)
progressBarDiscreteBlocksPassive w = passivePropertyNE w progressBarDiscreteBlocks


-- @A: progressBarEllipsize
progressBarEllipsizePassive :: ProgressBarClass self => (self) -> ReactiveFieldReadWrite IO (EllipsizeMode)
progressBarEllipsizePassive w = passivePropertyNE w progressBarEllipsize


-- @A: progressBarFraction
progressBarFractionPassive :: ProgressBarClass self => (self) -> ReactiveFieldReadWrite IO (Double)
progressBarFractionPassive w = passivePropertyNE w progressBarFraction


-- @G: progressBarGetEllipsize					 | ["progress","Bar","Get","Ellipsize"]
progressBarGetEllipsizePassive :: ProgressBarClass self => (self) -> ReactiveFieldRead IO (EllipsizeMode)
progressBarGetEllipsizePassive w = wrapMRPassive (progressBarGetEllipsize w)


-- @G: progressBarGetFraction					 | ["progress","Bar","Get","Fraction"]
progressBarGetFractionPassive :: ProgressBarClass self => (self) -> ReactiveFieldRead IO (Double)
progressBarGetFractionPassive w = wrapMRPassive (progressBarGetFraction w)


-- @G: progressBarGetOrientation					 | ["progress","Bar","Get","Orientation"]
progressBarGetOrientationPassive :: ProgressBarClass self => (self) -> ReactiveFieldRead IO (ProgressBarOrientation)
progressBarGetOrientationPassive w = wrapMRPassive (progressBarGetOrientation w)


-- @G: progressBarGetPulseStep					 | ["progress","Bar","Get","Pulse","Step"]
progressBarGetPulseStepPassive :: ProgressBarClass self => (self) -> ReactiveFieldRead IO (Double)
progressBarGetPulseStepPassive w = wrapMRPassive (progressBarGetPulseStep w)


-- @G: progressBarGetText					 | ["progress","Bar","Get","Text"]
progressBarGetTextPassive :: (ProgressBarClass self, GlibString string) => (self) -> ReactiveFieldRead IO ((Maybe string))
progressBarGetTextPassive w = wrapMRPassive (progressBarGetText w)


-- @A: progressBarOrientation
progressBarOrientationPassive :: ProgressBarClass self => (self) -> ReactiveFieldReadWrite IO (ProgressBarOrientation)
progressBarOrientationPassive w = passivePropertyNE w progressBarOrientation


-- @A: progressBarPulseStep
progressBarPulseStepPassive :: ProgressBarClass self => (self) -> ReactiveFieldReadWrite IO (Double)
progressBarPulseStepPassive w = passivePropertyNE w progressBarPulseStep


-- @T: progressBarSetEllipsize					 | ["progress","Bar","Set","Ellipsize"]
progressBarSetEllipsizeReactive :: ProgressBarClass self => (self) -> ReactiveFieldWrite IO (EllipsizeMode)
progressBarSetEllipsizeReactive w = wrapMW (progressBarSetEllipsize w)


-- @T: progressBarSetFraction					 | ["progress","Bar","Set","Fraction"]
progressBarSetFractionReactive :: ProgressBarClass self => (self) -> ReactiveFieldWrite IO (Double)
progressBarSetFractionReactive w = wrapMW (progressBarSetFraction w)


-- @T: progressBarSetOrientation					 | ["progress","Bar","Set","Orientation"]
progressBarSetOrientationReactive :: ProgressBarClass self => (self) -> ReactiveFieldWrite IO (ProgressBarOrientation)
progressBarSetOrientationReactive w = wrapMW (progressBarSetOrientation w)


-- @T: progressBarSetPulseStep					 | ["progress","Bar","Set","Pulse","Step"]
progressBarSetPulseStepReactive :: ProgressBarClass self => (self) -> ReactiveFieldWrite IO (Double)
progressBarSetPulseStepReactive w = wrapMW (progressBarSetPulseStep w)


-- @T: progressBarSetText					 | ["progress","Bar","Set","Text"]
progressBarSetTextReactive :: (ProgressBarClass self, GlibString string) => (self) -> ReactiveFieldWrite IO (string)
progressBarSetTextReactive w = wrapMW (progressBarSetText w)


-- @A: spinnerActive
spinnerActivePassive :: SpinnerClass spinner => (spinner) -> ReactiveFieldReadWrite IO (Bool)
spinnerActivePassive w = passivePropertyNE w spinnerActive


-- @C: afterTextPopped
-- TODO
-- @C: afterTextPushed
-- TODO
-- @C: onTextPopped
-- TODO
-- @C: onTextPushed
-- TODO
-- @G: statusbarGetContextId					 | ["statusbar","Get","Context","Id"]
-- TODO
-- @G: statusbarGetHasResizeGrip					 | ["statusbar","Get","Has","Resize","Grip"]
statusbarGetHasResizeGripPassive :: StatusbarClass self => (self) -> ReactiveFieldRead IO (Bool)
statusbarGetHasResizeGripPassive w = wrapMRPassive (statusbarGetHasResizeGrip w)


-- @G: statusbarGetMessageArea					 | ["statusbar","Get","Message","Area"]
statusbarGetMessageAreaPassive :: StatusbarClass self => (self) -> ReactiveFieldRead IO (Box)
statusbarGetMessageAreaPassive w = wrapMRPassive (statusbarGetMessageArea w)


-- @A: statusbarHasResizeGrip
statusbarHasResizeGripPassive :: StatusbarClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
statusbarHasResizeGripPassive w = passivePropertyNE w statusbarHasResizeGrip


-- @T: statusbarSetHasResizeGrip					 | ["statusbar","Set","Has","Resize","Grip"]
statusbarSetHasResizeGripReactive :: StatusbarClass self => (self) -> ReactiveFieldWrite IO (Bool)
statusbarSetHasResizeGripReactive w = wrapMW (statusbarSetHasResizeGrip w)


-- @S: textPopped
-- TODO
-- @S: textPushed
-- TODO
-- @C: afterSizeChanged
-- TODO
-- @C: onSizeChanged
-- TODO
-- @S: statusIconActivated
statusIconActivatedReactive :: StatusIconClass self => self -> ReactiveFieldRead IO ()
statusIconActivatedReactive = (`reactiveSignalIO` statusIconActivated)


-- @S: statusIconActivate
statusIconActivateReactive :: StatusIconClass self => self -> ReactiveFieldRead IO ()
statusIconActivateReactive = (`reactiveSignalIO` statusIconActivate)


-- @A: statusIconBlinking
statusIconBlinkingPassive :: StatusIconClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
statusIconBlinkingPassive w = passivePropertyNE w statusIconBlinking


-- @G: statusIconGetBlinking					 | ["status","Icon","Get","Blinking"]
statusIconGetBlinkingPassive :: StatusIconClass self => (self) -> ReactiveFieldRead IO (Bool)
statusIconGetBlinkingPassive w = wrapMRPassive (statusIconGetBlinking w)


-- @G: statusIconGetGeometry					 | ["status","Icon","Get","Geometry"]
statusIconGetGeometryPassive :: StatusIconClass self => (self) -> ReactiveFieldRead IO ((Maybe (Rectangle, Orientation)))
statusIconGetGeometryPassive w = wrapMRPassive (statusIconGetGeometry w)


-- @G: statusIconGetHasTooltip					 | ["status","Icon","Get","Has","Tooltip"]
statusIconGetHasTooltipPassive :: StatusIconClass self => (self) -> ReactiveFieldRead IO (Bool)
statusIconGetHasTooltipPassive w = wrapMRPassive (statusIconGetHasTooltip w)


-- @G: statusIconGetIconName					 | ["status","Icon","Get","Icon","Name"]
statusIconGetIconNamePassive :: (StatusIconClass self, GlibString string) => (self) -> ReactiveFieldRead IO ((Maybe string))
statusIconGetIconNamePassive w = wrapMRPassive (statusIconGetIconName w)


-- @G: statusIconGetPixbuf					 | ["status","Icon","Get","Pixbuf"]
statusIconGetPixbufPassive :: StatusIconClass self => (self) -> ReactiveFieldRead IO ((Maybe Pixbuf))
statusIconGetPixbufPassive w = wrapMRPassive (statusIconGetPixbuf w)


-- @G: statusIconGetScreen					 | ["status","Icon","Get","Screen"]
statusIconGetScreenPassive :: StatusIconClass self => (self) -> ReactiveFieldRead IO ((Maybe Screen))
statusIconGetScreenPassive w = wrapMRPassive (statusIconGetScreen w)


-- @G: statusIconGetSize					 | ["status","Icon","Get","Size"]
statusIconGetSizePassive :: StatusIconClass self => (self) -> ReactiveFieldRead IO (Int)
statusIconGetSizePassive w = wrapMRPassive (statusIconGetSize w)


-- @G: statusIconGetStock					 | ["status","Icon","Get","Stock"]
statusIconGetStockPassive :: StatusIconClass self => (self) -> ReactiveFieldRead IO ((Maybe StockId))
statusIconGetStockPassive w = wrapMRPassive (statusIconGetStock w)


-- @G: statusIconGetStorageType					 | ["status","Icon","Get","Storage","Type"]
statusIconGetStorageTypePassive :: StatusIconClass self => (self) -> ReactiveFieldRead IO (ImageType)
statusIconGetStorageTypePassive w = wrapMRPassive (statusIconGetStorageType w)


-- @G: statusIconGetTitle					 | ["status","Icon","Get","Title"]
statusIconGetTitlePassive :: (StatusIconClass self, GlibString string) => (self) -> ReactiveFieldRead IO ((Maybe string))
statusIconGetTitlePassive w = wrapMRPassive (statusIconGetTitle w)


-- @G: statusIconGetTooltipMarkup					 | ["status","Icon","Get","Tooltip","Markup"]
statusIconGetTooltipMarkupPassive :: (StatusIconClass self, GlibString string) => (self) -> ReactiveFieldRead IO ((Maybe string))
statusIconGetTooltipMarkupPassive w = wrapMRPassive (statusIconGetTooltipMarkup w)


-- @G: statusIconGetTooltipText					 | ["status","Icon","Get","Tooltip","Text"]
statusIconGetTooltipTextPassive :: (StatusIconClass self, GlibString string) => (self) -> ReactiveFieldRead IO ((Maybe string))
statusIconGetTooltipTextPassive w = wrapMRPassive (statusIconGetTooltipText w)


-- @G: statusIconGetVisible					 | ["status","Icon","Get","Visible"]
statusIconGetVisiblePassive :: StatusIconClass self => (self) -> ReactiveFieldRead IO (Bool)
statusIconGetVisiblePassive w = wrapMRPassive (statusIconGetVisible w)


-- @A: statusIconHasTooltip
statusIconHasTooltipPassive :: StatusIconClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
statusIconHasTooltipPassive w = passivePropertyNE w statusIconHasTooltip


-- @A: statusIconIconName
statusIconIconNamePassive :: (StatusIconClass self, GlibString string) => (self) -> ReactiveFieldReadWrite IO ((Maybe string))
statusIconIconNamePassive w = passivePropertyNE w statusIconIconName


-- @A: statusIconPixbuf
statusIconPixbufPassive :: StatusIconClass self => (self) -> ReactiveFieldReadWrite IO (Pixbuf)
statusIconPixbufPassive w = passivePropertyNE w statusIconPixbuf


-- @S: statusIconPopupMenu
-- TODO
-- @A: statusIconScreen
statusIconScreenPassive :: StatusIconClass self => (self) -> ReactiveFieldReadWrite IO (Screen)
statusIconScreenPassive w = passivePropertyNE w statusIconScreen


-- @T: statusIconSetBlinking					 | ["status","Icon","Set","Blinking"]
statusIconSetBlinkingReactive :: StatusIconClass self => (self) -> ReactiveFieldWrite IO (Bool)
statusIconSetBlinkingReactive w = wrapMW (statusIconSetBlinking w)


-- @T: statusIconSetFromFile					 | ["status","Icon","Set","From","File"]
statusIconSetFromFileReactive :: (StatusIconClass self, GlibString string) => (self) -> ReactiveFieldWrite IO (string)
statusIconSetFromFileReactive w = wrapMW (statusIconSetFromFile w)


-- @T: statusIconSetFromIconName					 | ["status","Icon","Set","From","Icon","Name"]
statusIconSetFromIconNameReactive :: (StatusIconClass self, GlibString string) => (self) -> ReactiveFieldWrite IO (string)
statusIconSetFromIconNameReactive w = wrapMW (statusIconSetFromIconName w)


-- @T: statusIconSetFromPixbuf					 | ["status","Icon","Set","From","Pixbuf"]
statusIconSetFromPixbufReactive :: StatusIconClass self => (self) -> ReactiveFieldWrite IO (Pixbuf)
statusIconSetFromPixbufReactive w = wrapMW (statusIconSetFromPixbuf w)


-- @T: statusIconSetFromStock					 | ["status","Icon","Set","From","Stock"]
statusIconSetFromStockReactive :: StatusIconClass self => (self) -> ReactiveFieldWrite IO (StockId)
statusIconSetFromStockReactive w = wrapMW (statusIconSetFromStock w)


-- @T: statusIconSetHasTooltip					 | ["status","Icon","Set","Has","Tooltip"]
statusIconSetHasTooltipReactive :: StatusIconClass self => (self) -> ReactiveFieldWrite IO (Bool)
statusIconSetHasTooltipReactive w = wrapMW (statusIconSetHasTooltip w)


-- @T: statusIconSetName					 | ["status","Icon","Set","Name"]
statusIconSetNameReactive :: (StatusIconClass self, GlibString string) => (self) -> ReactiveFieldWrite IO (string)
statusIconSetNameReactive w = wrapMW (statusIconSetName w)


-- @T: statusIconSetScreen					 | ["status","Icon","Set","Screen"]
statusIconSetScreenReactive :: (StatusIconClass self, ScreenClass screen) => (self) -> ReactiveFieldWrite IO (Maybe screen)
statusIconSetScreenReactive w = wrapMW (statusIconSetScreen w)


-- @T: statusIconSetTitle					 | ["status","Icon","Set","Title"]
statusIconSetTitleReactive :: (StatusIconClass self, GlibString string) => (self) -> ReactiveFieldWrite IO (Maybe string)
statusIconSetTitleReactive w = wrapMW (statusIconSetTitle w)


-- @T: statusIconSetTooltip					 | ["status","Icon","Set","Tooltip"]
statusIconSetTooltipReactive :: (StatusIconClass self, GlibString string) => (self) -> ReactiveFieldWrite IO (string)
statusIconSetTooltipReactive w = wrapMW (statusIconSetTooltip w)


-- @T: statusIconSetTooltipMarkup					 | ["status","Icon","Set","Tooltip","Markup"]
statusIconSetTooltipMarkupReactive :: (StatusIconClass self, GlibString string) => (self) -> ReactiveFieldWrite IO (Maybe string)
statusIconSetTooltipMarkupReactive w = wrapMW (statusIconSetTooltipMarkup w)


-- @T: statusIconSetTooltipText					 | ["status","Icon","Set","Tooltip","Text"]
statusIconSetTooltipTextReactive :: (StatusIconClass self, GlibString string) => (self) -> ReactiveFieldWrite IO (Maybe string)
statusIconSetTooltipTextReactive w = wrapMW (statusIconSetTooltipText w)


-- @T: statusIconSetVisible					 | ["status","Icon","Set","Visible"]
statusIconSetVisibleReactive :: StatusIconClass self => (self) -> ReactiveFieldWrite IO (Bool)
statusIconSetVisibleReactive w = wrapMW (statusIconSetVisible w)


-- @S: statusIconSizeChanged
-- TODO
-- @A: statusIconStock
statusIconStockPassive :: (StatusIconClass self, GlibString string) => (self) -> ReactiveFieldReadWrite IO ((Maybe string))
statusIconStockPassive w = passivePropertyNE w statusIconStock


-- @A: statusIconTitle
statusIconTitlePassive :: (StatusIconClass self, GlibString string) => (self) -> ReactiveFieldReadWrite IO ((Maybe string))
statusIconTitlePassive w = passivePropertyNE w statusIconTitle


-- @A: statusIconTooltipMarkup
statusIconTooltipMarkupPassive :: (StatusIconClass self, GlibString string) => (self) -> ReactiveFieldReadWrite IO ((Maybe string))
statusIconTooltipMarkupPassive w = passivePropertyNE w statusIconTooltipMarkup


-- @A: statusIconTooltipText
statusIconTooltipTextPassive :: (StatusIconClass self, GlibString string) => (self) -> ReactiveFieldReadWrite IO ((Maybe string))
statusIconTooltipTextPassive w = passivePropertyNE w statusIconTooltipText


-- @A: statusIconVisible
statusIconVisiblePassive :: StatusIconClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
statusIconVisiblePassive w = passivePropertyNE w statusIconVisible


-- @S: plugEmbedded
plugEmbeddedReactive :: PlugClass self => self -> ReactiveFieldRead IO ()
plugEmbeddedReactive = (`reactiveSignalIO` plugEmbedded)


-- @G: plugGetEmbedded					 | ["plug","Get","Embedded"]
plugGetEmbeddedPassive :: PlugClass self => (self) -> ReactiveFieldRead IO (Bool)
plugGetEmbeddedPassive w = wrapMRPassive (plugGetEmbedded w)


-- @G: plugGetId					 | ["plug","Get","Id"]
plugGetIdPassive :: PlugClass self => (self) -> ReactiveFieldRead IO (NativeWindowId)
plugGetIdPassive w = wrapMRPassive (plugGetId w)


-- @G: plugGetSocketWindow					 | ["plug","Get","Socket","Window"]
plugGetSocketWindowPassive :: PlugClass self => (self) -> ReactiveFieldRead IO ((Maybe DrawWindow))
plugGetSocketWindowPassive w = wrapMRPassive (plugGetSocketWindow w)


-- @C: afterPlugAdded
afterPlugAddedReactive :: SocketClass self => self -> ReactiveFieldRead IO ()
afterPlugAddedReactive w = reactivePropertyH_ w afterPlugAdded

-- @C: afterPlugRemoved
afterPlugRemovedReactive :: SocketClass self => self -> ReactiveFieldRead IO ()
afterPlugRemovedReactive w = reactivePropertyH_ w (\x i -> afterPlugRemoved x (i >> return False))

-- @C: onPlugAdded
onPlugAddedReactive :: SocketClass self => self -> ReactiveFieldRead IO ()
onPlugAddedReactive w = reactivePropertyH_ w onPlugAdded

-- @C: onPlugRemoved
onPlugRemovedReactive :: SocketClass self => self -> ReactiveFieldRead IO ()
onPlugRemovedReactive w = reactivePropertyH_ w (\x i -> onPlugRemoved x (i >> return False))

-- @G: socketGetId					 | ["socket","Get","Id"]
socketGetIdPassive :: SocketClass self => (self) -> ReactiveFieldRead IO (NativeWindowId)
socketGetIdPassive w = wrapMRPassive (socketGetId w)


-- @G: socketGetPlugWindow					 | ["socket","Get","Plug","Window"]
socketGetPlugWindowPassive :: SocketClass self => (self) -> ReactiveFieldRead IO ((Maybe DrawWindow))
socketGetPlugWindowPassive w = wrapMRPassive (socketGetPlugWindow w)


-- @S: socketPlugAdded
socketPlugAddedReactive :: SocketClass self => self -> ReactiveFieldRead IO ()
socketPlugAddedReactive = (`reactiveSignalIO` socketPlugAdded)


-- @S: socketPlugRemoved
-- TODO
-- @C: afterDeleteText
-- TODO
-- @C: afterEditableChanged
afterEditableChangedReactive :: EditableClass self => self -> ReactiveFieldRead IO ()
afterEditableChangedReactive w = reactivePropertyH_ w afterEditableChanged

-- @C: afterInsertText
-- TODO
-- @S: deleteText
-- TODO
-- @S: editableChanged
editableChangedReactive :: EditableClass self => self -> ReactiveFieldRead IO ()
editableChangedReactive = (`reactiveSignalIO` editableChanged)


-- @A: editableEditable
editableEditablePassive :: EditableClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
editableEditablePassive w = passivePropertyNE w editableEditable


-- @G: editableGetChars					 | ["editable","Get","Chars"]
-- TODO
-- @G: editableGetEditable					 | ["editable","Get","Editable"]
editableGetEditablePassive :: EditableClass self => (self) -> ReactiveFieldRead IO (Bool)
editableGetEditablePassive w = wrapMRPassive (editableGetEditable w)


-- @G: editableGetPosition					 | ["editable","Get","Position"]
editableGetPositionPassive :: EditableClass self => (self) -> ReactiveFieldRead IO (Int)
editableGetPositionPassive w = wrapMRPassive (editableGetPosition w)


-- @G: editableGetSelectionBounds					 | ["editable","Get","Selection","Bounds"]
editableGetSelectionBoundsPassive :: EditableClass self => (self) -> ReactiveFieldRead IO ((Int, Int))
editableGetSelectionBoundsPassive w = wrapMRPassive (editableGetSelectionBounds w)


-- @A: editablePosition
editablePositionPassive :: EditableClass self => (self) -> ReactiveFieldReadWrite IO (Int)
editablePositionPassive w = passivePropertyNE w editablePosition


-- @T: editableSetEditable					 | ["editable","Set","Editable"]
editableSetEditableReactive :: EditableClass self => (self) -> ReactiveFieldWrite IO (Bool)
editableSetEditableReactive w = wrapMW (editableSetEditable w)


-- @T: editableSetPosition					 | ["editable","Set","Position"]
editableSetPositionReactive :: EditableClass self => (self) -> ReactiveFieldWrite IO (Int)
editableSetPositionReactive w = wrapMW (editableSetPosition w)


-- @S: insertText
-- TODO
-- @C: onDeleteText
-- TODO
-- @C: onEditableChanged
onEditableChangedReactive :: EditableClass self => self -> ReactiveFieldRead IO ()
onEditableChangedReactive w = reactivePropertyH_ w onEditableChanged

-- @C: onInsertText
-- TODO
-- @C: afterCopyClipboard
afterCopyClipboardReactive :: EntryClass self => self -> ReactiveFieldRead IO ()
afterCopyClipboardReactive w = reactivePropertyH_ w afterCopyClipboard

-- @C: afterCutClipboard
afterCutClipboardReactive :: EntryClass self => self -> ReactiveFieldRead IO ()
afterCutClipboardReactive w = reactivePropertyH_ w afterCutClipboard

-- @C: afterEntryActivate
afterEntryActivateReactive :: EntryClass self => self -> ReactiveFieldRead IO ()
afterEntryActivateReactive w = reactivePropertyH_ w afterEntryActivate

-- @C: afterPasteClipboard
afterPasteClipboardReactive :: EntryClass self => self -> ReactiveFieldRead IO ()
afterPasteClipboardReactive w = reactivePropertyH_ w afterPasteClipboard

-- @C: afterToggleOverwrite
afterToggleOverwriteReactive :: EntryClass self => self -> ReactiveFieldRead IO ()
afterToggleOverwriteReactive w = reactivePropertyH_ w afterToggleOverwrite

-- @S: entryBufferDeletedText
-- TODO
-- @G: entryBufferGetBytes					 | ["entry","Buffer","Get","Bytes"]
entryBufferGetBytesPassive :: EntryBufferClass self => (self) -> ReactiveFieldRead IO (Int)
entryBufferGetBytesPassive w = wrapMRPassive (entryBufferGetBytes w)


-- @S: entryBufferInsertedText
-- TODO
-- @A: entryBufferMaxLength
entryBufferMaxLengthPassive :: EntryBufferClass self => (self) -> ReactiveFieldReadWrite IO (Int)
entryBufferMaxLengthPassive w = passivePropertyNE w entryBufferMaxLength


-- @A: entryBufferText
entryBufferTextPassive :: (EntryBufferClass self, GlibString string) => (self) -> ReactiveFieldReadWrite IO (string)
entryBufferTextPassive w = passivePropertyNE w entryBufferText


-- @C: afterActionActivated
-- TODO
-- @C: afterInsertPrefix
-- TODO
-- @S: completionActionActivated
-- TODO
-- @G: entryCompletionGetEntry					 | ["entry","Completion","Get","Entry"]
entryCompletionGetEntryPassive :: (EntryCompletion) -> ReactiveFieldRead IO ((Maybe Entry))
entryCompletionGetEntryPassive w = wrapMRPassive (entryCompletionGetEntry w)


-- @G: entryCompletionGetInlineCompletion					 | ["entry","Completion","Get","Inline","Completion"]
entryCompletionGetInlineCompletionPassive :: (EntryCompletion) -> ReactiveFieldRead IO (Bool)
entryCompletionGetInlineCompletionPassive w = wrapMRPassive (entryCompletionGetInlineCompletion w)


-- @G: entryCompletionGetMinimumKeyLength					 | ["entry","Completion","Get","Minimum","Key","Length"]
entryCompletionGetMinimumKeyLengthPassive :: (EntryCompletion) -> ReactiveFieldRead IO (Int)
entryCompletionGetMinimumKeyLengthPassive w = wrapMRPassive (entryCompletionGetMinimumKeyLength w)


-- @G: entryCompletionGetModel					 | ["entry","Completion","Get","Model"]
entryCompletionGetModelPassive :: (EntryCompletion) -> ReactiveFieldRead IO ((Maybe TreeModel))
entryCompletionGetModelPassive w = wrapMRPassive (entryCompletionGetModel w)


-- @G: entryCompletionGetPopupCompletion					 | ["entry","Completion","Get","Popup","Completion"]
entryCompletionGetPopupCompletionPassive :: (EntryCompletion) -> ReactiveFieldRead IO (Bool)
entryCompletionGetPopupCompletionPassive w = wrapMRPassive (entryCompletionGetPopupCompletion w)


-- @G: entryCompletionGetPopupSetWidth					 | ["entry","Completion","Get","Popup","Set","Width"]
entryCompletionGetPopupSetWidthPassive :: (EntryCompletion) -> ReactiveFieldRead IO (Bool)
entryCompletionGetPopupSetWidthPassive w = wrapMRPassive (entryCompletionGetPopupSetWidth w)


-- @G: entryCompletionGetPopupSingleMatch					 | ["entry","Completion","Get","Popup","Single","Match"]
entryCompletionGetPopupSingleMatchPassive :: (EntryCompletion) -> ReactiveFieldRead IO (Bool)
entryCompletionGetPopupSingleMatchPassive w = wrapMRPassive (entryCompletionGetPopupSingleMatch w)


-- @G: entryCompletionGetTextColumn					 | ["entry","Completion","Get","Text","Column"]
entryCompletionGetTextColumnPassive :: GlibString string => (EntryCompletion) -> ReactiveFieldRead IO ((ColumnId row string))
entryCompletionGetTextColumnPassive w = wrapMRPassive (entryCompletionGetTextColumn w)


-- @A: entryCompletionInlineCompletion
entryCompletionInlineCompletionPassive :: EntryCompletion -> ReactiveFieldReadWrite IO Bool
entryCompletionInlineCompletionPassive w = passivePropertyNE w entryCompletionInlineCompletion


-- @A: entryCompletionMinimumKeyLength
entryCompletionMinimumKeyLengthPassive :: EntryCompletion -> ReactiveFieldReadWrite IO Int
entryCompletionMinimumKeyLengthPassive w = passivePropertyNE w entryCompletionMinimumKeyLength


-- @A: entryCompletionPopupCompletion
entryCompletionPopupCompletionPassive :: EntryCompletion -> ReactiveFieldReadWrite IO Bool
entryCompletionPopupCompletionPassive w = passivePropertyNE w entryCompletionPopupCompletion


-- @A: entryCompletionPopupSetWidth
entryCompletionPopupSetWidthPassive :: EntryCompletion -> ReactiveFieldReadWrite IO Bool
entryCompletionPopupSetWidthPassive w = passivePropertyNE w entryCompletionPopupSetWidth


-- @A: entryCompletionPopupSingleMatch
entryCompletionPopupSingleMatchPassive :: EntryCompletion -> ReactiveFieldReadWrite IO Bool
entryCompletionPopupSingleMatchPassive w = passivePropertyNE w entryCompletionPopupSingleMatch


-- @T: entryCompletionSetInlineCompletion					 | ["entry","Completion","Set","Inline","Completion"]
entryCompletionSetInlineCompletionPassive :: (EntryCompletion) -> ReactiveFieldWrite IO (Bool)
entryCompletionSetInlineCompletionPassive w = wrapMW (entryCompletionSetInlineCompletion w)

-- @T: entryCompletionSetMatchFunc					 | ["entry","Completion","Set","Match","Func"]
entryCompletionSetMatchFuncReactive :: GlibString string => (EntryCompletion) -> ReactiveFieldWrite IO ((string -> TreeIter -> IO Bool))
entryCompletionSetMatchFuncReactive w = wrapMW (entryCompletionSetMatchFunc w)


-- @T: entryCompletionSetMinimumKeyLength					 | ["entry","Completion","Set","Minimum","Key","Length"]
entryCompletionSetMinimumKeyLengthPassive :: (EntryCompletion) -> ReactiveFieldWrite IO (Int)
entryCompletionSetMinimumKeyLengthPassive w = wrapMW (entryCompletionSetMinimumKeyLength w)

-- @T: entryCompletionSetModel					 | ["entry","Completion","Set","Model"]
entryCompletionSetModelReactive :: TreeModelClass model => (EntryCompletion) -> ReactiveFieldWrite IO (Maybe model)
entryCompletionSetModelReactive w = wrapMW (entryCompletionSetModel w)


-- @T: entryCompletionSetPopupCompletion					 | ["entry","Completion","Set","Popup","Completion"]
entryCompletionSetPopupCompletionPassive :: (EntryCompletion) -> ReactiveFieldWrite IO (Bool)
entryCompletionSetPopupCompletionPassive w = wrapMW (entryCompletionSetPopupCompletion w)

-- @T: entryCompletionSetPopupSetWidth					 | ["entry","Completion","Set","Popup","Set","Width"]
entryCompletionSetPopupSetWidthPassive :: (EntryCompletion) -> ReactiveFieldWrite IO (Bool)
entryCompletionSetPopupSetWidthPassive w = wrapMW (entryCompletionSetPopupSetWidth w)

-- @T: entryCompletionSetPopupSingleMatch					 | ["entry","Completion","Set","Popup","Single","Match"]
entryCompletionSetPopupSingleMatchPassive :: (EntryCompletion) -> ReactiveFieldWrite IO (Bool)
entryCompletionSetPopupSingleMatchPassive w = wrapMW (entryCompletionSetPopupSingleMatch w)

-- @T: entryCompletionSetTextColumn					 | ["entry","Completion","Set","Text","Column"]
entryCompletionSetTextColumnReactive :: GlibString string => (EntryCompletion) -> ReactiveFieldWrite IO (ColumnId row string)
entryCompletionSetTextColumnReactive w = wrapMW (entryCompletionSetTextColumn w)


-- @A: entryCompletionTextColumn
entryCompletionTextColumnPassive :: GlibString string => (EntryCompletion) -> ReactiveFieldReadWrite IO ((ColumnId row string))
entryCompletionTextColumnPassive w = passivePropertyNE w entryCompletionTextColumn


-- @S: insertPrefix
-- TODO
-- @S: matchSelected
-- TODO
-- @C: onActionActivated
-- TODO
-- @C: onInsertPrefix
-- TODO
-- @S: entryActivated
entryActivatedReactive :: EntryClass self => self -> ReactiveFieldRead IO ()
entryActivatedReactive = (`reactiveSignalIO` entryActivated)


-- @S: entryActivate
entryActivateReactive :: EntryClass self => self -> ReactiveFieldRead IO ()
entryActivateReactive = (`reactiveSignalIO` entryActivate)


-- @A: entryActivatesDefault
entryActivatesDefaultPassive :: EntryClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
entryActivatesDefaultPassive w = passivePropertyNE w entryActivatesDefault


-- @A: entryAlignment
entryAlignmentPassive :: EntryClass self => (self) -> ReactiveFieldReadWrite IO (Float)
entryAlignmentPassive w = passivePropertyNE w entryAlignment


-- @S: entryBackspace
entryBackspaceReactive :: EntryClass self => self -> ReactiveFieldRead IO ()
entryBackspaceReactive = (`reactiveSignalIO` entryBackspace)


-- @A: entryCompletion
entryCompletionPassive :: EntryClass self => (self) -> ReactiveFieldReadWrite IO (EntryCompletion)
entryCompletionPassive w = passivePropertyNE w entryCompletion


-- @S: entryCopyClipboard
entryCopyClipboardReactive :: EntryClass self => self -> ReactiveFieldRead IO ()
entryCopyClipboardReactive = (`reactiveSignalIO` entryCopyClipboard)


-- @S: entryCutClipboard
entryCutClipboardReactive :: EntryClass self => self -> ReactiveFieldRead IO ()
entryCutClipboardReactive = (`reactiveSignalIO` entryCutClipboard)


-- @S: entryDeleteFromCursor
-- TODO
-- @A: entryEditable
entryEditablePassive :: EntryClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
entryEditablePassive w = passivePropertyNE w entryEditable


-- @G: entryGetActivatesDefault					 | ["entry","Get","Activates","Default"]
entryGetActivatesDefaultPassive :: EntryClass self => (self) -> ReactiveFieldRead IO (Bool)
entryGetActivatesDefaultPassive w = wrapMRPassive (entryGetActivatesDefault w)


-- @G: entryGetAlignment					 | ["entry","Get","Alignment"]
entryGetAlignmentPassive :: EntryClass self => (self) -> ReactiveFieldRead IO (Float)
entryGetAlignmentPassive w = wrapMRPassive (entryGetAlignment w)


-- @G: entryGetBuffer					 | ["entry","Get","Buffer"]
entryGetBufferPassive :: EntryClass self => (self) -> ReactiveFieldRead IO (EntryBuffer)
entryGetBufferPassive w = wrapMRPassive (entryGetBuffer w)


-- @G: entryGetCompletion					 | ["entry","Get","Completion"]
entryGetCompletionPassive :: EntryClass self => (self) -> ReactiveFieldRead IO (EntryCompletion)
entryGetCompletionPassive w = wrapMRPassive (entryGetCompletion w)


-- @G: entryGetHasFrame					 | ["entry","Get","Has","Frame"]
entryGetHasFramePassive :: EntryClass self => (self) -> ReactiveFieldRead IO (Bool)
entryGetHasFramePassive w = wrapMRPassive (entryGetHasFrame w)


-- @G: entryGetIconWindow					 | ["entry","Get","Icon","Window"]
-- TODO
-- @G: entryGetInvisibleChar					 | ["entry","Get","Invisible","Char"]
entryGetInvisibleCharPassive :: EntryClass self => (self) -> ReactiveFieldRead IO (Char)
entryGetInvisibleCharPassive w = wrapMRPassive (entryGetInvisibleChar w)


-- @G: entryGetMaxLength					 | ["entry","Get","Max","Length"]
entryGetMaxLengthPassive :: EntryClass self => (self) -> ReactiveFieldRead IO (Int)
entryGetMaxLengthPassive w = wrapMRPassive (entryGetMaxLength w)


-- @G: entryGetText					 | ["entry","Get","Text"]
entryGetTextPassive :: (EntryClass self, GlibString string) => (self) -> ReactiveFieldRead IO (string)
entryGetTextPassive w = wrapMRPassive (entryGetText w)


-- @G: entryGetTextWindow					 | ["entry","Get","Text","Window"]
entryGetTextWindowPassive :: EntryClass self => (self) -> ReactiveFieldRead IO (DrawWindow)
entryGetTextWindowPassive w = wrapMRPassive (entryGetTextWindow w)


-- @G: entryGetVisibility					 | ["entry","Get","Visibility"]
entryGetVisibilityPassive :: EntryClass self => (self) -> ReactiveFieldRead IO (Bool)
entryGetVisibilityPassive w = wrapMRPassive (entryGetVisibility w)


-- @G: entryGetWidthChars					 | ["entry","Get","Width","Chars"]
entryGetWidthCharsPassive :: EntryClass self => (self) -> ReactiveFieldRead IO (Int)
entryGetWidthCharsPassive w = wrapMRPassive (entryGetWidthChars w)


-- @A: entryHasFrame
entryHasFramePassive :: EntryClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
entryHasFramePassive w = passivePropertyNE w entryHasFrame


-- @S: entryIconPress
-- TODO
-- @S: entryIconRelease
-- TODO
-- @S: entryInsertAtCursor
-- TODO
-- @A: entryInvisibleChar
entryInvisibleCharPassive :: EntryClass self => (self) -> ReactiveFieldReadWrite IO (Char)
entryInvisibleCharPassive w = passivePropertyNE w entryInvisibleChar


-- @A: entryMaxLength
entryMaxLengthPassive :: EntryClass self => (self) -> ReactiveFieldReadWrite IO (Int)
entryMaxLengthPassive w = passivePropertyNE w entryMaxLength


-- @S: entryMoveCursor
-- TODO
-- @S: entryPasteClipboard
entryPasteClipboardReactive :: EntryClass self => self -> ReactiveFieldRead IO ()
entryPasteClipboardReactive = (`reactiveSignalIO` entryPasteClipboard)


-- @S: entryPopulatePopup
-- TODO
-- @S: entryPreeditChanged
-- TODO
-- @T: entrySetActivatesDefault					 | ["entry","Set","Activates","Default"]
entrySetActivatesDefaultReactive :: EntryClass self => (self) -> ReactiveFieldWrite IO (Bool)
entrySetActivatesDefaultReactive w = wrapMW (entrySetActivatesDefault w)


-- @T: entrySetAlignment					 | ["entry","Set","Alignment"]
entrySetAlignmentReactive :: EntryClass self => (self) -> ReactiveFieldWrite IO (Float)
entrySetAlignmentReactive w = wrapMW (entrySetAlignment w)


-- @T: entrySetBuffer					 | ["entry","Set","Buffer"]
entrySetBufferReactive :: (EntryClass self, EntryBufferClass buffer) => (self) -> ReactiveFieldWrite IO (buffer)
entrySetBufferReactive w = wrapMW (entrySetBuffer w)


-- @T: entrySetCompletion					 | ["entry","Set","Completion"]
entrySetCompletionReactive :: EntryClass self => (self) -> ReactiveFieldWrite IO (EntryCompletion)
entrySetCompletionReactive w = wrapMW (entrySetCompletion w)


-- @T: entrySetHasFrame					 | ["entry","Set","Has","Frame"]
entrySetHasFrameReactive :: EntryClass self => (self) -> ReactiveFieldWrite IO (Bool)
entrySetHasFrameReactive w = wrapMW (entrySetHasFrame w)


-- @T: entrySetInvisibleChar					 | ["entry","Set","Invisible","Char"]
entrySetInvisibleCharReactive :: EntryClass self => (self) -> ReactiveFieldWrite IO (Char)
entrySetInvisibleCharReactive w = wrapMW (entrySetInvisibleChar w)


-- @T: entrySetMaxLength					 | ["entry","Set","Max","Length"]
entrySetMaxLengthReactive :: EntryClass self => (self) -> ReactiveFieldWrite IO (Int)
entrySetMaxLengthReactive w = wrapMW (entrySetMaxLength w)


-- @T: entrySetText					 | ["entry","Set","Text"]
entrySetTextReactive :: (EntryClass self, GlibString string) => (self) -> ReactiveFieldWrite IO (string)
entrySetTextReactive w = wrapMW (entrySetText w)


-- @T: entrySetVisibility					 | ["entry","Set","Visibility"]
entrySetVisibilityReactive :: EntryClass self => (self) -> ReactiveFieldWrite IO (Bool)
entrySetVisibilityReactive w = wrapMW (entrySetVisibility w)


-- @T: entrySetWidthChars					 | ["entry","Set","Width","Chars"]
entrySetWidthCharsReactive :: EntryClass self => (self) -> ReactiveFieldWrite IO (Int)
entrySetWidthCharsReactive w = wrapMW (entrySetWidthChars w)


-- @A: entryText
entryTextPassive :: (EntryClass self, GlibString string) => (self) -> ReactiveFieldReadWrite IO (string)
entryTextPassive w = passivePropertyNE w entryText


-- @S: entryToggleOverwirte
entryToggleOverwirteReactive :: EntryClass self => self -> ReactiveFieldRead IO ()
entryToggleOverwirteReactive = (`reactiveSignalIO` entryToggleOverwirte)


-- @S: entryToggleOverwrite
entryToggleOverwriteReactive :: EntryClass self => self -> ReactiveFieldRead IO ()
entryToggleOverwriteReactive = (`reactiveSignalIO` entryToggleOverwrite)


-- @A: entryVisibility
entryVisibilityPassive :: EntryClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
entryVisibilityPassive w = passivePropertyNE w entryVisibility


-- @A: entryWidthChars
entryWidthCharsPassive :: EntryClass self => (self) -> ReactiveFieldReadWrite IO (Int)
entryWidthCharsPassive w = passivePropertyNE w entryWidthChars


-- @A: entryXalign
entryXalignPassive :: EntryClass self => (self) -> ReactiveFieldReadWrite IO (Float)
entryXalignPassive w = passivePropertyNE w entryXalign


-- @C: onCopyClipboard
onCopyClipboardReactive :: EntryClass self => self -> ReactiveFieldRead IO ()
onCopyClipboardReactive w = reactivePropertyH_ w onCopyClipboard

-- @C: onCutClipboard
onCutClipboardReactive :: EntryClass self => self -> ReactiveFieldRead IO ()
onCutClipboardReactive w = reactivePropertyH_ w onCutClipboard

-- @C: onEntryActivate
onEntryActivateReactive :: EntryClass self => self -> ReactiveFieldRead IO ()
onEntryActivateReactive w = reactivePropertyH_ w onEntryActivate

-- @C: onPasteClipboard
onPasteClipboardReactive :: EntryClass self => self -> ReactiveFieldRead IO ()
onPasteClipboardReactive w = reactivePropertyH_ w onPasteClipboard

-- @C: onToggleOverwrite
onToggleOverwriteReactive :: EntryClass self => self -> ReactiveFieldRead IO ()
onToggleOverwriteReactive w = reactivePropertyH_ w onToggleOverwrite

-- @C: afterInput
-- TODO
-- @C: afterOutput
afterOutputReactive :: SpinButtonClass self => self -> ReactiveFieldRead IO ()
afterOutputReactive w = reactivePropertyH_ w (\x i -> afterOutput x (i >> return False))

-- @C: afterValueSpinned
afterValueSpinnedReactive :: SpinButtonClass self => self -> ReactiveFieldRead IO ()
afterValueSpinnedReactive w = reactivePropertyH_ w afterValueSpinned

-- @C: onInput
-- TODO
-- @C: onOutput
onOutputReactive :: SpinButtonClass self => self -> ReactiveFieldRead IO ()
onOutputReactive w = reactivePropertyH_ w (\x i -> onOutput x (i >> return False))

-- @C: onValueSpinned
onValueSpinnedReactive :: SpinButtonClass self => self -> ReactiveFieldRead IO ()
onValueSpinnedReactive w = reactivePropertyH_ w onValueSpinned

-- @A: spinButtonAdjustment
spinButtonAdjustmentPassive :: SpinButtonClass self => (self) -> ReactiveFieldReadWrite IO (Adjustment)
spinButtonAdjustmentPassive w = passivePropertyNE w spinButtonAdjustment


-- @A: spinButtonClimbRate
spinButtonClimbRatePassive :: SpinButtonClass self => (self) -> ReactiveFieldReadWrite IO (Double)
spinButtonClimbRatePassive w = passivePropertyNE w spinButtonClimbRate


-- @A: spinButtonDigits
spinButtonDigitsPassive :: SpinButtonClass self => (self) -> ReactiveFieldReadWrite IO (Int)
spinButtonDigitsPassive w = passivePropertyNE w spinButtonDigits


-- @G: spinButtonGetAdjustment					 | ["spin","Button","Get","Adjustment"]
spinButtonGetAdjustmentPassive :: SpinButtonClass self => (self) -> ReactiveFieldRead IO (Adjustment)
spinButtonGetAdjustmentPassive w = wrapMRPassive (spinButtonGetAdjustment w)


-- @G: spinButtonGetDigits					 | ["spin","Button","Get","Digits"]
spinButtonGetDigitsPassive :: SpinButtonClass self => (self) -> ReactiveFieldRead IO (Int)
spinButtonGetDigitsPassive w = wrapMRPassive (spinButtonGetDigits w)


-- @G: spinButtonGetIncrements					 | ["spin","Button","Get","Increments"]
spinButtonGetIncrementsPassive :: SpinButtonClass self => (self) -> ReactiveFieldRead IO ((Double, Double))
spinButtonGetIncrementsPassive w = wrapMRPassive (spinButtonGetIncrements w)


-- @G: spinButtonGetNumeric					 | ["spin","Button","Get","Numeric"]
spinButtonGetNumericPassive :: SpinButtonClass self => (self) -> ReactiveFieldRead IO (Bool)
spinButtonGetNumericPassive w = wrapMRPassive (spinButtonGetNumeric w)


-- @G: spinButtonGetRange					 | ["spin","Button","Get","Range"]
spinButtonGetRangePassive :: SpinButtonClass self => (self) -> ReactiveFieldRead IO ((Double, Double))
spinButtonGetRangePassive w = wrapMRPassive (spinButtonGetRange w)


-- @G: spinButtonGetSnapToTicks					 | ["spin","Button","Get","Snap","To","Ticks"]
spinButtonGetSnapToTicksPassive :: SpinButtonClass self => (self) -> ReactiveFieldRead IO (Bool)
spinButtonGetSnapToTicksPassive w = wrapMRPassive (spinButtonGetSnapToTicks w)


-- @G: spinButtonGetUpdatePolicy					 | ["spin","Button","Get","Update","Policy"]
spinButtonGetUpdatePolicyPassive :: SpinButtonClass self => (self) -> ReactiveFieldRead IO (SpinButtonUpdatePolicy)
spinButtonGetUpdatePolicyPassive w = wrapMRPassive (spinButtonGetUpdatePolicy w)


-- @G: spinButtonGetValueAsInt					 | ["spin","Button","Get","Value","As","Int"]
spinButtonGetValueAsIntPassive :: SpinButtonClass self => (self) -> ReactiveFieldRead IO (Int)
spinButtonGetValueAsIntPassive w = wrapMRPassive (spinButtonGetValueAsInt w)


-- @G: spinButtonGetValue					 | ["spin","Button","Get","Value"]
spinButtonGetValuePassive :: SpinButtonClass self => (self) -> ReactiveFieldRead IO (Double)
spinButtonGetValuePassive w = wrapMRPassive (spinButtonGetValue w)


-- @G: spinButtonGetWrap					 | ["spin","Button","Get","Wrap"]
spinButtonGetWrapPassive :: SpinButtonClass self => (self) -> ReactiveFieldRead IO (Bool)
spinButtonGetWrapPassive w = wrapMRPassive (spinButtonGetWrap w)


-- @A: spinButtonNumeric
spinButtonNumericPassive :: SpinButtonClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
spinButtonNumericPassive w = passivePropertyNE w spinButtonNumeric


-- @T: spinButtonSetAdjustment					 | ["spin","Button","Set","Adjustment"]
spinButtonSetAdjustmentReactive :: SpinButtonClass self => (self) -> ReactiveFieldWrite IO (Adjustment)
spinButtonSetAdjustmentReactive w = wrapMW (spinButtonSetAdjustment w)


-- @T: spinButtonSetDigits					 | ["spin","Button","Set","Digits"]
spinButtonSetDigitsReactive :: SpinButtonClass self => (self) -> ReactiveFieldWrite IO (Int)
spinButtonSetDigitsReactive w = wrapMW (spinButtonSetDigits w)


-- @T: spinButtonSetIncrements					 | ["spin","Button","Set","Increments"]
-- TODO
-- @T: spinButtonSetNumeric					 | ["spin","Button","Set","Numeric"]
spinButtonSetNumericReactive :: SpinButtonClass self => (self) -> ReactiveFieldWrite IO (Bool)
spinButtonSetNumericReactive w = wrapMW (spinButtonSetNumeric w)


-- @T: spinButtonSetRange					 | ["spin","Button","Set","Range"]
-- TODO
-- @T: spinButtonSetSnapToTicks					 | ["spin","Button","Set","Snap","To","Ticks"]
spinButtonSetSnapToTicksReactive :: SpinButtonClass self => (self) -> ReactiveFieldWrite IO (Bool)
spinButtonSetSnapToTicksReactive w = wrapMW (spinButtonSetSnapToTicks w)


-- @T: spinButtonSetUpdatePolicy					 | ["spin","Button","Set","Update","Policy"]
spinButtonSetUpdatePolicyReactive :: SpinButtonClass self => (self) -> ReactiveFieldWrite IO (SpinButtonUpdatePolicy)
spinButtonSetUpdatePolicyReactive w = wrapMW (spinButtonSetUpdatePolicy w)


-- @T: spinButtonSetValue					 | ["spin","Button","Set","Value"]
spinButtonSetValueReactive :: SpinButtonClass self => (self) -> ReactiveFieldWrite IO (Double)
spinButtonSetValueReactive w = wrapMW (spinButtonSetValue w)


-- @T: spinButtonSetWrap					 | ["spin","Button","Set","Wrap"]
spinButtonSetWrapReactive :: SpinButtonClass self => (self) -> ReactiveFieldWrite IO (Bool)
spinButtonSetWrapReactive w = wrapMW (spinButtonSetWrap w)


-- @A: spinButtonSnapToTicks
spinButtonSnapToTicksPassive :: SpinButtonClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
spinButtonSnapToTicksPassive w = passivePropertyNE w spinButtonSnapToTicks


-- @A: spinButtonUpdatePolicy
spinButtonUpdatePolicyPassive :: SpinButtonClass self => (self) -> ReactiveFieldReadWrite IO (SpinButtonUpdatePolicy)
spinButtonUpdatePolicyPassive w = passivePropertyNE w spinButtonUpdatePolicy


-- @A: spinButtonValue
spinButtonValuePassive :: SpinButtonClass self => (self) -> ReactiveFieldReadWrite IO (Double)
spinButtonValuePassive w = passivePropertyNE w spinButtonValue


-- @A: spinButtonWrap
spinButtonWrapPassive :: SpinButtonClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
spinButtonWrapPassive w = passivePropertyNE w spinButtonWrap


-- @T: appLaunchContextSetDesktop					 | ["app","Launch","Context","Set","Desktop"]
appLaunchContextSetDesktopPassive :: (AppLaunchContext) -> ReactiveFieldWrite IO (Int)
appLaunchContextSetDesktopPassive w = wrapMW (appLaunchContextSetDesktop w)

-- @T: appLaunchContextSetDisplay					 | ["app","Launch","Context","Set","Display"]
appLaunchContextSetDisplayPassive :: (AppLaunchContext) -> ReactiveFieldWrite IO (Display)
appLaunchContextSetDisplayPassive w = wrapMW (appLaunchContextSetDisplay w)

-- @T: appLaunchContextSetIconName					 | ["app","Launch","Context","Set","Icon","Name"]
appLaunchContextSetIconNameReactive :: GlibString string => (AppLaunchContext) -> ReactiveFieldWrite IO (string)
appLaunchContextSetIconNameReactive w = wrapMW (appLaunchContextSetIconName w)


-- @T: appLaunchContextSetIcon					 | ["app","Launch","Context","Set","Icon"]

appLaunchContextSetIconReactive w = wrapMW (appLaunchContextSetIcon w)


-- @T: appLaunchContextSetScreen					 | ["app","Launch","Context","Set","Screen"]
appLaunchContextSetScreenPassive :: (AppLaunchContext) -> ReactiveFieldWrite IO (Screen)
appLaunchContextSetScreenPassive w = wrapMW (appLaunchContextSetScreen w)

-- @T: appLaunchContextSetTimestamp					 | ["app","Launch","Context","Set","Timestamp"]
appLaunchContextSetTimestampPassive :: (AppLaunchContext) -> ReactiveFieldWrite IO (TimeStamp)
appLaunchContextSetTimestampPassive w = wrapMW (appLaunchContextSetTimestamp w)

-- @G: cursorGetDisplay					 | ["cursor","Get","Display"]
cursorGetDisplayPassive :: (Cursor) -> ReactiveFieldRead IO (Display)
cursorGetDisplayPassive w = wrapMRPassive (cursorGetDisplay w)


-- @G: cursorGetImage					 | ["cursor","Get","Image"]
cursorGetImagePassive :: (Cursor) -> ReactiveFieldRead IO ((Maybe Pixbuf))
cursorGetImagePassive w = wrapMRPassive (cursorGetImage w)


-- @S: displayClosed
-- TODO
-- @G: displayGetDefaultCursorSize					 | ["display","Get","Default","Cursor","Size"]
displayGetDefaultCursorSizePassive :: (Display) -> ReactiveFieldRead IO (Int)
displayGetDefaultCursorSizePassive w = wrapMRPassive (displayGetDefaultCursorSize w)


-- @G: displayGetDefaultGroup					 | ["display","Get","Default","Group"]
displayGetDefaultGroupPassive :: (Display) -> ReactiveFieldRead IO (DrawWindow)
displayGetDefaultGroupPassive w = wrapMRPassive (displayGetDefaultGroup w)


-- @G: displayGetDefault					 | ["display","Get","Default"]
displayGetDefaultPassive :: ReactiveFieldRead IO ((Maybe Display))
displayGetDefaultPassive = wrapMRPassive (displayGetDefault)


-- @G: displayGetDefaultScreen					 | ["display","Get","Default","Screen"]
displayGetDefaultScreenPassive :: (Display) -> ReactiveFieldRead IO (Screen)
displayGetDefaultScreenPassive w = wrapMRPassive (displayGetDefaultScreen w)


-- @G: displayGetMaximalCursorSize					 | ["display","Get","Maximal","Cursor","Size"]
displayGetMaximalCursorSizePassive :: (Display) -> ReactiveFieldRead IO ((Int, Int))
displayGetMaximalCursorSizePassive w = wrapMRPassive (displayGetMaximalCursorSize w)


-- @G: displayGetName					 | ["display","Get","Name"]
displayGetNamePassive :: GlibString string => (Display) -> ReactiveFieldRead IO (string)
displayGetNamePassive w = wrapMRPassive (displayGetName w)


-- @G: displayGetNScreens					 | ["display","Get","NScreens"]
displayGetNScreensPassive :: (Display) -> ReactiveFieldRead IO (Int)
displayGetNScreensPassive w = wrapMRPassive (displayGetNScreens w)


-- @G: displayGetPointer					 | ["display","Get","Pointer"]
displayGetPointerPassive :: (Display) -> ReactiveFieldRead IO ((Screen, [Modifier], Int, Int))
displayGetPointerPassive w = wrapMRPassive (displayGetPointer w)


-- @G: displayGetScreen					 | ["display","Get","Screen"]
-- TODO
-- @G: displayGetWindowAtPointer					 | ["display","Get","Window","At","Pointer"]
displayGetWindowAtPointerPassive :: (Display) -> ReactiveFieldRead IO ((Maybe (DrawWindow, Int, Int)))
displayGetWindowAtPointerPassive w = wrapMRPassive (displayGetWindowAtPointer w)


-- @T: displaySetDoubleClickDistance					 | ["display","Set","Double","Click","Distance"]
displaySetDoubleClickDistancePassive :: (Display) -> ReactiveFieldWrite IO (Int)
displaySetDoubleClickDistancePassive w = wrapMW (displaySetDoubleClickDistance w)

-- @T: displaySetDoubleClickTime					 | ["display","Set","Double","Click","Time"]
displaySetDoubleClickTimePassive :: (Display) -> ReactiveFieldWrite IO (Int)
displaySetDoubleClickTimePassive w = wrapMW (displaySetDoubleClickTime w)

-- @A: displayManagerDefaultDisplay
displayManagerDefaultDisplayPassive :: DisplayManagerClass self => (self) -> ReactiveFieldReadWrite IO (Display)
displayManagerDefaultDisplayPassive w = passivePropertyNE w displayManagerDefaultDisplay


-- @G: displayManagerGet					 | ["display","Manager","Get"]
displayManagerGetPassive :: ReactiveFieldRead IO (DisplayManager)
displayManagerGetPassive = wrapMRPassive (displayManagerGet)


-- @S: displayManagerOpened
-- TODO
-- @G: drawableGetClipRegion					 | ["drawable","Get","Clip","Region"]
drawableGetClipRegionPassive :: DrawableClass d => (d) -> ReactiveFieldRead IO (Region)
drawableGetClipRegionPassive w = wrapMRPassive (drawableGetClipRegion w)


-- @G: drawableGetDepth					 | ["drawable","Get","Depth"]
drawableGetDepthPassive :: DrawableClass d => (d) -> ReactiveFieldRead IO (Int)
drawableGetDepthPassive w = wrapMRPassive (drawableGetDepth w)


-- @G: drawableGetSize					 | ["drawable","Get","Size"]
drawableGetSizePassive :: DrawableClass d => (d) -> ReactiveFieldRead IO ((Int, Int))
drawableGetSizePassive w = wrapMRPassive (drawableGetSize w)


-- @G: drawableGetVisibleRegion					 | ["drawable","Get","Visible","Region"]
drawableGetVisibleRegionPassive :: DrawableClass d => (d) -> ReactiveFieldRead IO (Region)
drawableGetVisibleRegionPassive w = wrapMRPassive (drawableGetVisibleRegion w)


-- @G: drawWindowGetDefaultRootWindow					 | ["draw","Window","Get","Default","Root","Window"]
drawWindowGetDefaultRootWindowPassive :: ReactiveFieldRead IO (DrawWindow)
drawWindowGetDefaultRootWindowPassive = wrapMRPassive (drawWindowGetDefaultRootWindow)


-- @G: drawWindowGetHeight					 | ["draw","Window","Get","Height"]
drawWindowGetHeightPassive :: (DrawWindow) -> ReactiveFieldRead IO (Int)
drawWindowGetHeightPassive w = wrapMRPassive (drawWindowGetHeight w)


-- @G: drawWindowGetOrigin					 | ["draw","Window","Get","Origin"]
drawWindowGetOriginPassive :: (DrawWindow) -> ReactiveFieldRead IO ((Int, Int))
drawWindowGetOriginPassive w = wrapMRPassive (drawWindowGetOrigin w)


-- @G: drawWindowGetPointer					 | ["draw","Window","Get","Pointer"]
drawWindowGetPointerPassive :: DrawWindowClass self => (self) -> ReactiveFieldRead IO ((Maybe (Bool, Int, Int, [Modifier])))
drawWindowGetPointerPassive w = wrapMRPassive (drawWindowGetPointer w)


-- @G: drawWindowGetPointerPos					 | ["draw","Window","Get","Pointer","Pos"]
drawWindowGetPointerPosPassive :: DrawWindowClass self => (self) -> ReactiveFieldRead IO ((Maybe DrawWindow, Int, Int, [Modifier]))
drawWindowGetPointerPosPassive w = wrapMRPassive (drawWindowGetPointerPos w)


-- @G: drawWindowGetState					 | ["draw","Window","Get","State"]
drawWindowGetStatePassive :: DrawWindowClass self => (self) -> ReactiveFieldRead IO ([WindowState])
drawWindowGetStatePassive w = wrapMRPassive (drawWindowGetState w)


-- @G: drawWindowGetUpdateArea					 | ["draw","Window","Get","Update","Area"]
drawWindowGetUpdateAreaPassive :: DrawWindowClass self => (self) -> ReactiveFieldRead IO ((Maybe Region))
drawWindowGetUpdateAreaPassive w = wrapMRPassive (drawWindowGetUpdateArea w)


-- @G: drawWindowGetWidth					 | ["draw","Window","Get","Width"]
drawWindowGetWidthPassive :: (DrawWindow) -> ReactiveFieldRead IO (Int)
drawWindowGetWidthPassive w = wrapMRPassive (drawWindowGetWidth w)


-- @T: drawWindowSetAcceptFocus					 | ["draw","Window","Set","Accept","Focus"]
drawWindowSetAcceptFocusReactive :: DrawWindowClass self => (self) -> ReactiveFieldWrite IO (Bool)
drawWindowSetAcceptFocusReactive w = wrapMW (drawWindowSetAcceptFocus w)


-- @T: drawWindowSetChildShapes					 | ["draw","Window","Set","Child","Shapes"]
-- TODO
-- @T: drawWindowSetCursor					 | ["draw","Window","Set","Cursor"]
drawWindowSetCursorPassive :: (DrawWindow) -> ReactiveFieldWrite IO (Maybe Cursor)
drawWindowSetCursorPassive w = wrapMW (drawWindowSetCursor w)

-- @S: keymapDirectionChanged
keymapDirectionChangedReactive :: KeymapClass self => self -> ReactiveFieldRead IO ()
keymapDirectionChangedReactive = (`reactiveSignalIO` keymapDirectionChanged)


-- @G: keymapGetCapsLockState					 | ["keymap","Get","Caps","Lock","State"]
keymapGetCapsLockStatePassive :: KeymapClass self => (self) -> ReactiveFieldRead IO (Bool)
keymapGetCapsLockStatePassive w = wrapMRPassive (keymapGetCapsLockState w)


-- @G: keymapGetDefault					 | ["keymap","Get","Default"]
keymapGetDefaultPassive :: ReactiveFieldRead IO (Keymap)
keymapGetDefaultPassive = wrapMRPassive (keymapGetDefault)


-- @G: keymapGetDirection					 | ["keymap","Get","Direction"]
keymapGetDirectionPassive :: KeymapClass self => (self) -> ReactiveFieldRead IO (PangoDirection)
keymapGetDirectionPassive w = wrapMRPassive (keymapGetDirection w)


-- @G: keymapGetEntriesForKeycode					 | ["keymap","Get","Entries","For","Keycode"]
-- TODO
-- @G: keymapGetEntriesForKeyval					 | ["keymap","Get","Entries","For","Keyval"]
-- TODO
-- @G: keymapGetForDisplay					 | ["keymap","Get","For","Display"]
keymapGetForDisplayPassive :: (Display) -> ReactiveFieldRead IO (Keymap)
keymapGetForDisplayPassive w = wrapMRPassive (keymapGetForDisplay w)


-- @S: keymapKeysChanged
keymapKeysChangedReactive :: KeymapClass self => self -> ReactiveFieldRead IO ()
keymapKeysChangedReactive = (`reactiveSignalIO` keymapKeysChanged)


-- @S: keymapStateChanged
keymapStateChangedReactive :: KeymapClass self => self -> ReactiveFieldRead IO ()
keymapStateChangedReactive = (`reactiveSignalIO` keymapStateChanged)


-- @G: pixbufGetBitsPerSample					 | ["pixbuf","Get","Bits","Per","Sample"]
pixbufGetBitsPerSamplePassive :: (Pixbuf) -> ReactiveFieldRead IO (Int)
pixbufGetBitsPerSamplePassive w = wrapMRPassive (pixbufGetBitsPerSample w)


-- @G: pixbufGetColorSpace					 | ["pixbuf","Get","Color","Space"]
pixbufGetColorSpacePassive :: (Pixbuf) -> ReactiveFieldRead IO (Colorspace)
pixbufGetColorSpacePassive w = wrapMRPassive (pixbufGetColorSpace w)


-- @G: pixbufGetFormats					 | ["pixbuf","Get","Formats"]
-- TODO
-- @G: pixbufGetFromDrawable					 | ["pixbuf","Get","From","Drawable"]
-- TODO
-- @G: pixbufGetHasAlpha					 | ["pixbuf","Get","Has","Alpha"]
pixbufGetHasAlphaPassive :: (Pixbuf) -> ReactiveFieldRead IO (Bool)
pixbufGetHasAlphaPassive w = wrapMRPassive (pixbufGetHasAlpha w)


-- @G: pixbufGetHeight					 | ["pixbuf","Get","Height"]
pixbufGetHeightPassive :: (Pixbuf) -> ReactiveFieldRead IO (Int)
pixbufGetHeightPassive w = wrapMRPassive (pixbufGetHeight w)


-- @G: pixbufGetNChannels					 | ["pixbuf","Get","NChannels"]
pixbufGetNChannelsPassive :: (Pixbuf) -> ReactiveFieldRead IO (Int)
pixbufGetNChannelsPassive w = wrapMRPassive (pixbufGetNChannels w)


-- @G: pixbufGetOption					 | ["pixbuf","Get","Option"]
-- TODO
-- @G: pixbufGetPixels					 | ["pixbuf","Get","Pixels"]

pixbufGetPixelsPassive w = wrapMRPassive (pixbufGetPixels w)


-- @G: pixbufGetRowstride					 | ["pixbuf","Get","Rowstride"]
pixbufGetRowstridePassive :: (Pixbuf) -> ReactiveFieldRead IO (Int)
pixbufGetRowstridePassive w = wrapMRPassive (pixbufGetRowstride w)


-- @G: pixbufGetWidth					 | ["pixbuf","Get","Width"]
pixbufGetWidthPassive :: (Pixbuf) -> ReactiveFieldRead IO (Int)
pixbufGetWidthPassive w = wrapMRPassive (pixbufGetWidth w)


-- @G: regionGetClipbox					 | ["region","Get","Clipbox"]
regionGetClipboxPassive :: (Region) -> ReactiveFieldRead IO (Rectangle)
regionGetClipboxPassive w = wrapMRPassive (regionGetClipbox w)


-- @G: regionGetRectangles					 | ["region","Get","Rectangles"]
regionGetRectanglesPassive :: (Region) -> ReactiveFieldRead IO ([Rectangle])
regionGetRectanglesPassive w = wrapMRPassive (regionGetRectangles w)


-- @S: screenCompositedChanged
screenCompositedChangedReactive :: ScreenClass self => self -> ReactiveFieldRead IO ()
screenCompositedChangedReactive = (`reactiveSignalIO` screenCompositedChanged)


-- @A: screenDefaultColormap

screenDefaultColormapPassive w = passivePropertyNE w screenDefaultColormap


-- @A: screenFontOptions
screenFontOptionsPassive :: Screen -> ReactiveFieldReadWrite IO ((Maybe FontOptions))
screenFontOptionsPassive w = passivePropertyNE w screenFontOptions


-- @G: screenGetActiveWindow					 | ["screen","Get","Active","Window"]
screenGetActiveWindowPassive :: (Screen) -> ReactiveFieldRead IO ((Maybe DrawWindow))
screenGetActiveWindowPassive w = wrapMRPassive (screenGetActiveWindow w)


-- @G: screenGetDefaultColormap					 | ["screen","Get","Default","Colormap"]

screenGetDefaultColormapPassive w = wrapMRPassive (screenGetDefaultColormap w)


-- @G: screenGetDefault					 | ["screen","Get","Default"]
screenGetDefaultPassive :: ReactiveFieldRead IO ((Maybe Screen))
screenGetDefaultPassive = wrapMRPassive (screenGetDefault)


-- @G: screenGetDisplay					 | ["screen","Get","Display"]
screenGetDisplayPassive :: (Screen) -> ReactiveFieldRead IO (Display)
screenGetDisplayPassive w = wrapMRPassive (screenGetDisplay w)


-- @G: screenGetHeight					 | ["screen","Get","Height"]
screenGetHeightPassive :: (Screen) -> ReactiveFieldRead IO (Int)
screenGetHeightPassive w = wrapMRPassive (screenGetHeight w)


-- @G: screenGetHeightMm					 | ["screen","Get","Height","Mm"]
screenGetHeightMmPassive :: (Screen) -> ReactiveFieldRead IO (Int)
screenGetHeightMmPassive w = wrapMRPassive (screenGetHeightMm w)


-- @G: screenGetHeightMM					 | ["screen","Get","Height","MM"]
screenGetHeightMMPassive :: (Screen) -> ReactiveFieldRead IO (Int)
screenGetHeightMMPassive w = wrapMRPassive (screenGetHeightMM w)


-- @G: screenGetMonitorAtPoint					 | ["screen","Get","Monitor","At","Point"]
-- TODO
-- @G: screenGetMonitorAtWindow					 | ["screen","Get","Monitor","At","Window"]
-- TODO
-- @G: screenGetMonitorGeometry					 | ["screen","Get","Monitor","Geometry"]
-- TODO
-- @G: screenGetMonitorHeightMm					 | ["screen","Get","Monitor","Height","Mm"]
-- TODO
-- @G: screenGetMonitorPlugName					 | ["screen","Get","Monitor","Plug","Name"]
-- TODO
-- @G: screenGetMonitorWidthMm					 | ["screen","Get","Monitor","Width","Mm"]
-- TODO
-- @G: screenGetNMonitors					 | ["screen","Get","NMonitors"]
screenGetNMonitorsPassive :: (Screen) -> ReactiveFieldRead IO (Int)
screenGetNMonitorsPassive w = wrapMRPassive (screenGetNMonitors w)


-- @G: screenGetNumber					 | ["screen","Get","Number"]
screenGetNumberPassive :: (Screen) -> ReactiveFieldRead IO (Int)
screenGetNumberPassive w = wrapMRPassive (screenGetNumber w)


-- @G: screenGetRGBAColormap					 | ["screen","Get","RGBAColormap"]

screenGetRGBAColormapPassive w = wrapMRPassive (screenGetRGBAColormap w)


-- @G: screenGetRootWindow					 | ["screen","Get","Root","Window"]
screenGetRootWindowPassive :: (Screen) -> ReactiveFieldRead IO (DrawWindow)
screenGetRootWindowPassive w = wrapMRPassive (screenGetRootWindow w)


-- @G: screenGetSystemColormap					 | ["screen","Get","System","Colormap"]

screenGetSystemColormapPassive w = wrapMRPassive (screenGetSystemColormap w)


-- @G: screenGetSystemVisual					 | ["screen","Get","System","Visual"]

screenGetSystemVisualPassive w = wrapMRPassive (screenGetSystemVisual w)


-- @G: screenGetToplevelWindows					 | ["screen","Get","Toplevel","Windows"]
screenGetToplevelWindowsPassive :: (Screen) -> ReactiveFieldRead IO ([DrawWindow])
screenGetToplevelWindowsPassive w = wrapMRPassive (screenGetToplevelWindows w)


-- @G: screenGetWidth					 | ["screen","Get","Width"]
screenGetWidthPassive :: (Screen) -> ReactiveFieldRead IO (Int)
screenGetWidthPassive w = wrapMRPassive (screenGetWidth w)


-- @G: screenGetWidthMm					 | ["screen","Get","Width","Mm"]
screenGetWidthMmPassive :: (Screen) -> ReactiveFieldRead IO (Int)
screenGetWidthMmPassive w = wrapMRPassive (screenGetWidthMm w)


-- @G: screenGetWidthMM					 | ["screen","Get","Width","MM"]
screenGetWidthMMPassive :: (Screen) -> ReactiveFieldRead IO (Int)
screenGetWidthMMPassive w = wrapMRPassive (screenGetWidthMM w)


-- @G: screenGetWindowStack					 | ["screen","Get","Window","Stack"]
screenGetWindowStackPassive :: (Screen) -> ReactiveFieldRead IO ((Maybe [DrawWindow]))
screenGetWindowStackPassive w = wrapMRPassive (screenGetWindowStack w)


-- @S: screenMonitorsChanged
screenMonitorsChangedReactive :: ScreenClass self => self -> ReactiveFieldRead IO ()
screenMonitorsChangedReactive = (`reactiveSignalIO` screenMonitorsChanged)


-- @A: screenResolution
screenResolutionPassive :: Screen -> ReactiveFieldReadWrite IO Double
screenResolutionPassive w = passivePropertyNE w screenResolution


-- @T: screenSetDefaultColormap					 | ["screen","Set","Default","Colormap"]

screenSetDefaultColormapPassive w = wrapMW (screenSetDefaultColormap w)

-- @S: screenSizeChanged
screenSizeChangedReactive :: ScreenClass self => self -> ReactiveFieldRead IO ()
screenSizeChangedReactive = (`reactiveSignalIO` screenSizeChanged)


-- @G: clipboardGetDisplay					 | ["clipboard","Get","Display"]
clipboardGetDisplayPassive :: ClipboardClass self => (self) -> ReactiveFieldRead IO (Display)
clipboardGetDisplayPassive w = wrapMRPassive (clipboardGetDisplay w)


-- @G: clipboardGetForDisplay					 | ["clipboard","Get","For","Display"]
-- TODO
-- @G: clipboardGet					 | ["clipboard","Get"]
clipboardGetPassive :: (SelectionTag) -> ReactiveFieldRead IO (Clipboard)
clipboardGetPassive w = wrapMRPassive (clipboardGet w)


-- @T: clipboardSetCanStore					 | ["clipboard","Set","Can","Store"]
clipboardSetCanStoreReactive :: ClipboardClass self => (self) -> ReactiveFieldWrite IO (Maybe [(TargetTag, InfoId)])
clipboardSetCanStoreReactive w = wrapMW (clipboardSetCanStore w)


-- @T: clipboardSetImage					 | ["clipboard","Set","Image"]
clipboardSetImageReactive :: ClipboardClass self => (self) -> ReactiveFieldWrite IO (Pixbuf)
clipboardSetImageReactive w = wrapMW (clipboardSetImage w)


-- @T: clipboardSetText					 | ["clipboard","Set","Text"]
clipboardSetTextReactive :: (ClipboardClass self, GlibString string) => (self) -> ReactiveFieldWrite IO (string)
clipboardSetTextReactive w = wrapMW (clipboardSetText w)


-- @T: clipboardSetWithData					 | ["clipboard","Set","With","Data"]
-- TODO
-- @S: dragBegin
-- TODO
-- @A: dragContextActions
dragContextActionsPassive :: DragContext -> ReactiveFieldReadWrite IO ([DragAction])
dragContextActionsPassive w = passivePropertyNE w dragContextActions


-- @A: dragContextAction
dragContextActionPassive :: DragContext -> ReactiveFieldReadWrite IO DragAction
dragContextActionPassive w = passivePropertyNE w dragContextAction


-- @A: dragContextSuggestedAction
dragContextSuggestedActionPassive :: DragContext -> ReactiveFieldReadWrite IO DragAction
dragContextSuggestedActionPassive w = passivePropertyNE w dragContextSuggestedAction


-- @S: dragDataDelete
-- TODO
-- @S: dragDataGet
-- TODO
-- @S: dragDataReceived
-- TODO
-- @G: dragDestGetTargetList					 | ["drag","Dest","Get","Target","List"]
dragDestGetTargetListPassive :: WidgetClass widget => (widget) -> ReactiveFieldRead IO ((Maybe TargetList))
dragDestGetTargetListPassive w = wrapMRPassive (dragDestGetTargetList w)


-- @T: dragDestSet					 | ["drag","Dest","Set"]
-- TODO
-- @T: dragDestSetProxy					 | ["drag","Dest","Set","Proxy"]
-- TODO
-- @T: dragDestSetTargetList					 | ["drag","Dest","Set","Target","List"]
dragDestSetTargetListReactive :: WidgetClass widget => (widget) -> ReactiveFieldWrite IO (TargetList)
dragDestSetTargetListReactive w = wrapMW (dragDestSetTargetList w)


-- @S: dragDrop
-- TODO
-- @S: dragEnd
-- TODO
-- @S: dragFailed
-- TODO
-- @G: dragGetData					 | ["drag","Get","Data"]
-- TODO
-- @G: dragGetSourceWidget					 | ["drag","Get","Source","Widget"]
dragGetSourceWidgetPassive :: DragContextClass context => (context) -> ReactiveFieldRead IO ((Maybe Widget))
dragGetSourceWidgetPassive w = wrapMRPassive (dragGetSourceWidget w)


-- @S: dragLeave
-- TODO
-- @S: dragMotion
-- TODO
-- @T: dragSetIconDefault					 | ["drag","Set","Icon","Default"]
-- TODO
-- @T: dragSetIconName					 | ["drag","Set","Icon","Name"]
-- TODO
-- @T: dragSetIconPixbuf					 | ["drag","Set","Icon","Pixbuf"]
-- TODO
-- @T: dragSetIconStock					 | ["drag","Set","Icon","Stock"]
-- TODO
-- @T: dragSetIconWidget					 | ["drag","Set","Icon","Widget"]
-- TODO
-- @G: dragSourceGetTargetList					 | ["drag","Source","Get","Target","List"]
dragSourceGetTargetListPassive :: WidgetClass widget => (widget) -> ReactiveFieldRead IO ((Maybe TargetList))
dragSourceGetTargetListPassive w = wrapMRPassive (dragSourceGetTargetList w)


-- @T: dragSourceSet					 | ["drag","Source","Set"]
-- TODO
-- @T: dragSourceSetIconName					 | ["drag","Source","Set","Icon","Name"]
dragSourceSetIconNameReactive :: (WidgetClass widget, GlibString string) => (widget) -> ReactiveFieldWrite IO (string)
dragSourceSetIconNameReactive w = wrapMW (dragSourceSetIconName w)


-- @T: dragSourceSetIconPixbuf					 | ["drag","Source","Set","Icon","Pixbuf"]
dragSourceSetIconPixbufReactive :: WidgetClass widget => (widget) -> ReactiveFieldWrite IO (Pixbuf)
dragSourceSetIconPixbufReactive w = wrapMW (dragSourceSetIconPixbuf w)


-- @T: dragSourceSetIconStock					 | ["drag","Source","Set","Icon","Stock"]
dragSourceSetIconStockReactive :: WidgetClass widget => (widget) -> ReactiveFieldWrite IO (StockId)
dragSourceSetIconStockReactive w = wrapMW (dragSourceSetIconStock w)


-- @T: dragSourceSetTargetList					 | ["drag","Source","Set","Target","List"]
dragSourceSetTargetListReactive :: WidgetClass widget => (widget) -> ReactiveFieldWrite IO (TargetList)
dragSourceSetTargetListReactive w = wrapMW (dragSourceSetTargetList w)


-- @G: grabGetCurrent					 | ["grab","Get","Current"]
grabGetCurrentPassive :: ReactiveFieldRead IO ((Maybe Widget))
grabGetCurrentPassive = wrapMRPassive (grabGetCurrent)


-- @T: iconSetAddSource					 | ["icon","Set","Add","Source"]
iconSetAddSourcePassive :: (IconSet) -> ReactiveFieldWrite IO (IconSource)
iconSetAddSourcePassive w = wrapMW (iconSetAddSource w)

-- @G: iconSetGetSizes					 | ["icon","Set","Get","Sizes"]
iconSetGetSizesPassive :: (IconSet) -> ReactiveFieldRead IO ([IconSize])
iconSetGetSizesPassive w = wrapMRPassive (iconSetGetSizes w)


-- @T: iconSetNewFromPixbuf					 | ["icon","Set","New","From","Pixbuf"]
-- TODO
-- @T: iconSetNew					 | ["icon","Set","New"]
-- TODO
-- @T: iconSetRenderIcon					 | ["icon","Set","Render","Icon"]
-- TODO
-- @G: iconSizeGetName					 | ["icon","Size","Get","Name"]
iconSizeGetNamePassive :: GlibString string => (IconSize) -> ReactiveFieldRead IO ((Maybe string))
iconSizeGetNamePassive w = wrapMRPassive (iconSizeGetName w)


-- @G: iconSourceGetDirection					 | ["icon","Source","Get","Direction"]
iconSourceGetDirectionPassive :: (IconSource) -> ReactiveFieldRead IO ((Maybe TextDirection))
iconSourceGetDirectionPassive w = wrapMRPassive (iconSourceGetDirection w)


-- @G: iconSourceGetFilename					 | ["icon","Source","Get","Filename"]
iconSourceGetFilenamePassive :: GlibString string => (IconSource) -> ReactiveFieldRead IO ((Maybe string))
iconSourceGetFilenamePassive w = wrapMRPassive (iconSourceGetFilename w)


-- @G: iconSourceGetPixbuf					 | ["icon","Source","Get","Pixbuf"]
iconSourceGetPixbufPassive :: (IconSource) -> ReactiveFieldRead IO ((Maybe Pixbuf))
iconSourceGetPixbufPassive w = wrapMRPassive (iconSourceGetPixbuf w)


-- @G: iconSourceGetSize					 | ["icon","Source","Get","Size"]
iconSourceGetSizePassive :: (IconSource) -> ReactiveFieldRead IO ((Maybe IconSize))
iconSourceGetSizePassive w = wrapMRPassive (iconSourceGetSize w)


-- @G: iconSourceGetState					 | ["icon","Source","Get","State"]
iconSourceGetStatePassive :: (IconSource) -> ReactiveFieldRead IO ((Maybe StateType))
iconSourceGetStatePassive w = wrapMRPassive (iconSourceGetState w)


-- @T: iconSourceSetDirection					 | ["icon","Source","Set","Direction"]
iconSourceSetDirectionPassive :: (IconSource) -> ReactiveFieldWrite IO (TextDirection)
iconSourceSetDirectionPassive w = wrapMW (iconSourceSetDirection w)

-- @T: iconSourceSetFilename					 | ["icon","Source","Set","Filename"]
iconSourceSetFilenameReactive :: GlibFilePath fp => (IconSource) -> ReactiveFieldWrite IO (fp)
iconSourceSetFilenameReactive w = wrapMW (iconSourceSetFilename w)


-- @T: iconSourceSetPixbuf					 | ["icon","Source","Set","Pixbuf"]
iconSourceSetPixbufPassive :: (IconSource) -> ReactiveFieldWrite IO (Pixbuf)
iconSourceSetPixbufPassive w = wrapMW (iconSourceSetPixbuf w)

-- @T: iconSourceSetSize					 | ["icon","Source","Set","Size"]
iconSourceSetSizePassive :: (IconSource) -> ReactiveFieldWrite IO (IconSize)
iconSourceSetSizePassive w = wrapMW (iconSourceSetSize w)

-- @T: iconSourceSetState					 | ["icon","Source","Set","State"]
iconSourceSetStatePassive :: (IconSource) -> ReactiveFieldWrite IO (StateType)
iconSourceSetStatePassive w = wrapMW (iconSourceSetState w)

-- @G: iconInfoGetAttachPoints					 | ["icon","Info","Get","Attach","Points"]
iconInfoGetAttachPointsPassive :: (IconInfo) -> ReactiveFieldRead IO ((Maybe [Point]))
iconInfoGetAttachPointsPassive w = wrapMRPassive (iconInfoGetAttachPoints w)


-- @G: iconInfoGetBaseSize					 | ["icon","Info","Get","Base","Size"]
iconInfoGetBaseSizePassive :: (IconInfo) -> ReactiveFieldRead IO (Int)
iconInfoGetBaseSizePassive w = wrapMRPassive (iconInfoGetBaseSize w)


-- @G: iconInfoGetBuiltinPixbuf					 | ["icon","Info","Get","Builtin","Pixbuf"]
iconInfoGetBuiltinPixbufPassive :: (IconInfo) -> ReactiveFieldRead IO ((Maybe Pixbuf))
iconInfoGetBuiltinPixbufPassive w = wrapMRPassive (iconInfoGetBuiltinPixbuf w)


-- @G: iconInfoGetDisplayName					 | ["icon","Info","Get","Display","Name"]
iconInfoGetDisplayNamePassive :: GlibString string => (IconInfo) -> ReactiveFieldRead IO ((Maybe string))
iconInfoGetDisplayNamePassive w = wrapMRPassive (iconInfoGetDisplayName w)


-- @G: iconInfoGetEmbeddedRect					 | ["icon","Info","Get","Embedded","Rect"]
iconInfoGetEmbeddedRectPassive :: (IconInfo) -> ReactiveFieldRead IO ((Maybe Rectangle))
iconInfoGetEmbeddedRectPassive w = wrapMRPassive (iconInfoGetEmbeddedRect w)


-- @G: iconInfoGetFilename					 | ["icon","Info","Get","Filename"]
iconInfoGetFilenamePassive :: GlibString string => (IconInfo) -> ReactiveFieldRead IO ((Maybe string))
iconInfoGetFilenamePassive w = wrapMRPassive (iconInfoGetFilename w)


-- @T: iconInfoSetRawCoordinates					 | ["icon","Info","Set","Raw","Coordinates"]
iconInfoSetRawCoordinatesPassive :: (IconInfo) -> ReactiveFieldWrite IO (Bool)
iconInfoSetRawCoordinatesPassive w = wrapMW (iconInfoSetRawCoordinates w)

-- @S: iconThemeChanged
iconThemeChangedReactive :: IconThemeClass self => self -> ReactiveFieldRead IO ()
iconThemeChangedReactive = (`reactiveSignalIO` iconThemeChanged)


-- @G: iconThemeGetDefault					 | ["icon","Theme","Get","Default"]
iconThemeGetDefaultPassive :: ReactiveFieldRead IO (IconTheme)
iconThemeGetDefaultPassive = wrapMRPassive (iconThemeGetDefault)


-- @G: iconThemeGetExampleIconName					 | ["icon","Theme","Get","Example","Icon","Name"]
iconThemeGetExampleIconNamePassive :: (IconThemeClass self, GlibString string) => (self) -> ReactiveFieldRead IO ((Maybe string))
iconThemeGetExampleIconNamePassive w = wrapMRPassive (iconThemeGetExampleIconName w)


-- @G: iconThemeGetForScreen					 | ["icon","Theme","Get","For","Screen"]
iconThemeGetForScreenPassive :: (Screen) -> ReactiveFieldRead IO (IconTheme)
iconThemeGetForScreenPassive w = wrapMRPassive (iconThemeGetForScreen w)


-- @G: iconThemeGetIconSizes					 | ["icon","Theme","Get","Icon","Sizes"]
-- TODO
-- @G: iconThemeGetSearchPath					 | ["icon","Theme","Get","Search","Path"]
iconThemeGetSearchPathPassive :: (IconThemeClass self, GlibFilePath fp) => (self) -> ReactiveFieldRead IO (([fp], Int))
iconThemeGetSearchPathPassive w = wrapMRPassive (iconThemeGetSearchPath w)


-- @T: iconThemeSetCustomTheme					 | ["icon","Theme","Set","Custom","Theme"]
iconThemeSetCustomThemeReactive :: (IconThemeClass self, GlibString string) => (self) -> ReactiveFieldWrite IO (Maybe string)
iconThemeSetCustomThemeReactive w = wrapMW (iconThemeSetCustomTheme w)


-- @T: iconThemeSetScreen					 | ["icon","Theme","Set","Screen"]
iconThemeSetScreenReactive :: IconThemeClass self => (self) -> ReactiveFieldWrite IO (Screen)
iconThemeSetScreenReactive w = wrapMW (iconThemeSetScreen w)


-- @T: iconThemeSetSearchPath					 | ["icon","Theme","Set","Search","Path"]
-- TODO
-- @G: rcGetDefaultFiles					 | ["rc","Get","Default","Files"]
-- TODO
-- @G: rcGetImModuleFile					 | ["rc","Get","Im","Module","File"]
-- TODO
-- @G: rcGetModuleDir					 | ["rc","Get","Module","Dir"]
-- TODO
-- @G: rcGetStyleByPaths					 | ["rc","Get","Style","By","Paths"]
-- TODO
-- @G: rcGetStyle					 | ["rc","Get","Style"]
rcGetStylePassive :: WidgetClass widget => (widget) -> ReactiveFieldRead IO (Style)
rcGetStylePassive w = wrapMRPassive (rcGetStyle w)


-- @G: rcGetThemeDir					 | ["rc","Get","Theme","Dir"]
-- TODO
-- @T: rcSetDefaultFiles					 | ["rc","Set","Default","Files"]
-- TODO
-- @G: selectionDataGet					 | ["selection","Data","Get"]
-- TODO
-- @G: selectionDataGetPixbuf					 | ["selection","Data","Get","Pixbuf"]
-- TODO
-- @G: selectionDataGetTarget					 | ["selection","Data","Get","Target"]
-- TODO
-- @G: selectionDataGetTargets					 | ["selection","Data","Get","Targets"]
-- TODO
-- @G: selectionDataGetText					 | ["selection","Data","Get","Text"]
-- TODO
-- @G: selectionDataGetURIs					 | ["selection","Data","Get","URIs"]
-- TODO
-- @T: selectionDataSet					 | ["selection","Data","Set"]
-- TODO
-- @T: selectionDataSetPixbuf					 | ["selection","Data","Set","Pixbuf"]
-- TODO
-- @T: selectionDataSetTarget					 | ["selection","Data","Set","Target"]
-- TODO
-- @T: selectionDataSetText					 | ["selection","Data","Set","Text"]
-- TODO
-- @T: selectionDataSetURIs					 | ["selection","Data","Set","URIs"]
-- TODO
-- @S: selectionGet
-- TODO
-- @T: selectionOwnerSetForDisplay					 | ["selection","Owner","Set","For","Display"]
-- TODO
-- @T: selectionOwnerSet					 | ["selection","Owner","Set"]
-- TODO
-- @S: selectionReceived
-- TODO
-- @G: settingsGetDefault					 | ["settings","Get","Default"]
settingsGetDefaultPassive :: ReactiveFieldRead IO ((Maybe Settings))
settingsGetDefaultPassive = wrapMRPassive (settingsGetDefault)


-- @G: settingsGetForScreen					 | ["settings","Get","For","Screen"]
settingsGetForScreenPassive :: ScreenClass screen => (screen) -> ReactiveFieldRead IO (Settings)
settingsGetForScreenPassive w = wrapMRPassive (settingsGetForScreen w)


-- @T: settingsSetLongProperty					 | ["settings","Set","Long","Property"]
-- TODO
-- @T: settingsSetStringProperty					 | ["settings","Set","String","Property"]
-- TODO
-- @G: colorSelectionDialogGetCancelButton					 | ["color","Selection","Dialog","Get","Cancel","Button"]
colorSelectionDialogGetCancelButtonPassive :: (ColorSelectionDialog) -> ReactiveFieldRead IO (Button)
colorSelectionDialogGetCancelButtonPassive w = wrapMRPassive (colorSelectionDialogGetCancelButton w)


-- @G: colorSelectionDialogGetColor					 | ["color","Selection","Dialog","Get","Color"]
colorSelectionDialogGetColorPassive :: (ColorSelectionDialog) -> ReactiveFieldRead IO (ColorSelection)
colorSelectionDialogGetColorPassive w = wrapMRPassive (colorSelectionDialogGetColor w)


-- @G: colorSelectionDialogGetHelpButton					 | ["color","Selection","Dialog","Get","Help","Button"]
colorSelectionDialogGetHelpButtonPassive :: (ColorSelectionDialog) -> ReactiveFieldRead IO (Button)
colorSelectionDialogGetHelpButtonPassive w = wrapMRPassive (colorSelectionDialogGetHelpButton w)


-- @G: colorSelectionDialogGetOkButton					 | ["color","Selection","Dialog","Get","Ok","Button"]
colorSelectionDialogGetOkButtonPassive :: (ColorSelectionDialog) -> ReactiveFieldRead IO (Button)
colorSelectionDialogGetOkButtonPassive w = wrapMRPassive (colorSelectionDialogGetOkButton w)


-- @G: dialogGetActionArea					 | ["dialog","Get","Action","Area"]
dialogGetActionAreaPassive :: DialogClass dc => (dc) -> ReactiveFieldRead IO (HBox)
dialogGetActionAreaPassive w = wrapMRPassive (dialogGetActionArea w)


-- @G: dialogGetUpper					 | ["dialog","Get","Upper"]
dialogGetUpperPassive :: DialogClass dc => (dc) -> ReactiveFieldRead IO (VBox)
dialogGetUpperPassive w = wrapMRPassive (dialogGetUpper w)


-- @G: drawableGetID					 | ["drawable","Get","ID"]
drawableGetIDPassive :: DrawableClass d => (d) -> ReactiveFieldRead IO (NativeWindowId)
drawableGetIDPassive w = wrapMRPassive (drawableGetID w)


-- @G: fileSelectionGetButtons					 | ["file","Selection","Get","Buttons"]
fileSelectionGetButtonsPassive :: FileSelectionClass fsel => (fsel) -> ReactiveFieldRead IO ((Button, Button))
fileSelectionGetButtonsPassive w = wrapMRPassive (fileSelectionGetButtons w)


-- @G: styleGetAntiAliasing					 | ["style","Get","Anti","Aliasing"]
-- TODO
-- @G: styleGetBackground					 | ["style","Get","Background"]
-- TODO
-- @G: styleGetBase					 | ["style","Get","Base"]
-- TODO
-- @G: styleGetDark					 | ["style","Get","Dark"]
-- TODO
-- @G: styleGetForeground					 | ["style","Get","Foreground"]
-- TODO
-- @G: styleGetLight					 | ["style","Get","Light"]
-- TODO
-- @G: styleGetMiddle					 | ["style","Get","Middle"]
-- TODO
-- @G: styleGetText					 | ["style","Get","Text"]
-- TODO
-- @G: widgetGetDrawWindow					 | ["widget","Get","Draw","Window"]
widgetGetDrawWindowPassive :: WidgetClass widget => (widget) -> ReactiveFieldRead IO (DrawWindow)
widgetGetDrawWindowPassive w = wrapMRPassive (widgetGetDrawWindow w)


-- @G: widgetGetSavedState					 | ["widget","Get","Saved","State"]
widgetGetSavedStatePassive :: WidgetClass w => (w) -> ReactiveFieldRead IO (StateType)
widgetGetSavedStatePassive w = wrapMRPassive (widgetGetSavedState w)


-- @G: widgetGetSize					 | ["widget","Get","Size"]
widgetGetSizePassive :: WidgetClass widget => (widget) -> ReactiveFieldRead IO ((Int, Int))
widgetGetSizePassive w = wrapMRPassive (widgetGetSize w)


-- @G: windowGetFrame					 | ["window","Get","Frame"]
windowGetFramePassive :: WindowClass widget => (widget) -> ReactiveFieldRead IO ((Maybe DrawWindow))
windowGetFramePassive w = wrapMRPassive (windowGetFrame w)


-- @A: alignmentBottomPadding
alignmentBottomPaddingPassive :: AlignmentClass self => (self) -> ReactiveFieldReadWrite IO (Int)
alignmentBottomPaddingPassive w = passivePropertyNE w alignmentBottomPadding


-- @G: alignmentGetPadding					 | ["alignment","Get","Padding"]
alignmentGetPaddingPassive :: AlignmentClass self => (self) -> ReactiveFieldRead IO ((Int, Int, Int, Int))
alignmentGetPaddingPassive w = wrapMRPassive (alignmentGetPadding w)


-- @A: alignmentLeftPadding
alignmentLeftPaddingPassive :: AlignmentClass self => (self) -> ReactiveFieldReadWrite IO (Int)
alignmentLeftPaddingPassive w = passivePropertyNE w alignmentLeftPadding


-- @A: alignmentRightPadding
alignmentRightPaddingPassive :: AlignmentClass self => (self) -> ReactiveFieldReadWrite IO (Int)
alignmentRightPaddingPassive w = passivePropertyNE w alignmentRightPadding


-- @T: alignmentSet					 | ["alignment","Set"]
-- TODO
-- @T: alignmentSetPadding					 | ["alignment","Set","Padding"]
-- TODO
-- @A: alignmentTopPadding
alignmentTopPaddingPassive :: AlignmentClass self => (self) -> ReactiveFieldReadWrite IO (Int)
alignmentTopPaddingPassive w = passivePropertyNE w alignmentTopPadding


-- @A: alignmentXAlign
alignmentXAlignPassive :: AlignmentClass self => (self) -> ReactiveFieldReadWrite IO (Float)
alignmentXAlignPassive w = passivePropertyNE w alignmentXAlign


-- @A: alignmentXScale
alignmentXScalePassive :: AlignmentClass self => (self) -> ReactiveFieldReadWrite IO (Float)
alignmentXScalePassive w = passivePropertyNE w alignmentXScale


-- @A: alignmentYAlign
alignmentYAlignPassive :: AlignmentClass self => (self) -> ReactiveFieldReadWrite IO (Float)
alignmentYAlignPassive w = passivePropertyNE w alignmentYAlign


-- @A: alignmentYScale
alignmentYScalePassive :: AlignmentClass self => (self) -> ReactiveFieldReadWrite IO (Float)
alignmentYScalePassive w = passivePropertyNE w alignmentYScale


-- @A: aspectFrameObeyChild
aspectFrameObeyChildPassive :: AspectFrameClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
aspectFrameObeyChildPassive w = passivePropertyNE w aspectFrameObeyChild


-- @A: aspectFrameRatio
aspectFrameRatioPassive :: AspectFrameClass self => (self) -> ReactiveFieldReadWrite IO (Float)
aspectFrameRatioPassive w = passivePropertyNE w aspectFrameRatio


-- @T: aspectFrameSet					 | ["aspect","Frame","Set"]
-- TODO
-- @A: aspectFrameXAlign
aspectFrameXAlignPassive :: AspectFrameClass self => (self) -> ReactiveFieldReadWrite IO (Float)
aspectFrameXAlignPassive w = passivePropertyNE w aspectFrameXAlign


-- @A: aspectFrameYAlign
aspectFrameYAlignPassive :: AspectFrameClass self => (self) -> ReactiveFieldReadWrite IO (Float)
aspectFrameYAlignPassive w = passivePropertyNE w aspectFrameYAlign


-- @C: afterActivate
afterActivateReactive :: Expander -> ReactiveFieldRead IO ()
afterActivateReactive w = reactivePropertyH_ w afterActivate

-- @A: expanderExpanded
expanderExpandedPassive :: Expander -> ReactiveFieldReadWrite IO Bool
expanderExpandedPassive w = passivePropertyNE w expanderExpanded


-- @G: expanderGetExpanded					 | ["expander","Get","Expanded"]
expanderGetExpandedPassive :: (Expander) -> ReactiveFieldRead IO (Bool)
expanderGetExpandedPassive w = wrapMRPassive (expanderGetExpanded w)


-- @G: expanderGetLabel					 | ["expander","Get","Label"]
expanderGetLabelPassive :: GlibString string => (Expander) -> ReactiveFieldRead IO (string)
expanderGetLabelPassive w = wrapMRPassive (expanderGetLabel w)


-- @G: expanderGetLabelWidget					 | ["expander","Get","Label","Widget"]
expanderGetLabelWidgetPassive :: (Expander) -> ReactiveFieldRead IO (Widget)
expanderGetLabelWidgetPassive w = wrapMRPassive (expanderGetLabelWidget w)


-- @G: expanderGetSpacing					 | ["expander","Get","Spacing"]
expanderGetSpacingPassive :: (Expander) -> ReactiveFieldRead IO (Int)
expanderGetSpacingPassive w = wrapMRPassive (expanderGetSpacing w)


-- @G: expanderGetUseMarkup					 | ["expander","Get","Use","Markup"]
expanderGetUseMarkupPassive :: (Expander) -> ReactiveFieldRead IO (Bool)
expanderGetUseMarkupPassive w = wrapMRPassive (expanderGetUseMarkup w)


-- @G: expanderGetUseUnderline					 | ["expander","Get","Use","Underline"]
expanderGetUseUnderlinePassive :: (Expander) -> ReactiveFieldRead IO (Bool)
expanderGetUseUnderlinePassive w = wrapMRPassive (expanderGetUseUnderline w)


-- @A: expanderLabelFill
expanderLabelFillPassive :: Expander -> ReactiveFieldReadWrite IO Bool
expanderLabelFillPassive w = passivePropertyNE w expanderLabelFill


-- @A: expanderLabel
expanderLabelPassive :: GlibString string => Expander -> ReactiveFieldReadWrite IO string
expanderLabelPassive w = passivePropertyNE w expanderLabel


-- @T: expanderSetExpanded					 | ["expander","Set","Expanded"]
expanderSetExpandedPassive :: (Expander) -> ReactiveFieldWrite IO (Bool)
expanderSetExpandedPassive w = wrapMW (expanderSetExpanded w)

-- @T: expanderSetLabel					 | ["expander","Set","Label"]
expanderSetLabelReactive :: GlibString string => (Expander) -> ReactiveFieldWrite IO (string)
expanderSetLabelReactive w = wrapMW (expanderSetLabel w)


-- @T: expanderSetLabelWidget					 | ["expander","Set","Label","Widget"]
expanderSetLabelWidgetReactive :: WidgetClass labelWidget => (Expander) -> ReactiveFieldWrite IO (labelWidget)
expanderSetLabelWidgetReactive w = wrapMW (expanderSetLabelWidget w)


-- @T: expanderSetSpacing					 | ["expander","Set","Spacing"]
expanderSetSpacingPassive :: (Expander) -> ReactiveFieldWrite IO (Int)
expanderSetSpacingPassive w = wrapMW (expanderSetSpacing w)

-- @T: expanderSetUseMarkup					 | ["expander","Set","Use","Markup"]
expanderSetUseMarkupPassive :: (Expander) -> ReactiveFieldWrite IO (Bool)
expanderSetUseMarkupPassive w = wrapMW (expanderSetUseMarkup w)

-- @T: expanderSetUseUnderline					 | ["expander","Set","Use","Underline"]
expanderSetUseUnderlinePassive :: (Expander) -> ReactiveFieldWrite IO (Bool)
expanderSetUseUnderlinePassive w = wrapMW (expanderSetUseUnderline w)

-- @A: expanderSpacing
expanderSpacingPassive :: Expander -> ReactiveFieldReadWrite IO Int
expanderSpacingPassive w = passivePropertyNE w expanderSpacing


-- @A: expanderUseMarkup
expanderUseMarkupPassive :: Expander -> ReactiveFieldReadWrite IO Bool
expanderUseMarkupPassive w = passivePropertyNE w expanderUseMarkup


-- @A: expanderUseUnderline
expanderUseUnderlinePassive :: Expander -> ReactiveFieldReadWrite IO Bool
expanderUseUnderlinePassive w = passivePropertyNE w expanderUseUnderline


-- @C: onActivate
onActivateReactive :: Expander -> ReactiveFieldRead IO ()
onActivateReactive w = reactivePropertyH_ w onActivate

-- @A: fixedChildX
-- TODO
-- @A: fixedChildY
-- TODO
-- @G: fixedGetHasWindow					 | ["fixed","Get","Has","Window"]
fixedGetHasWindowPassive :: FixedClass self => (self) -> ReactiveFieldRead IO (Bool)
fixedGetHasWindowPassive w = wrapMRPassive (fixedGetHasWindow w)


-- @A: fixedHasWindow
fixedHasWindowPassive :: FixedClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
fixedHasWindowPassive w = passivePropertyNE w fixedHasWindow


-- @T: fixedSetHasWindow					 | ["fixed","Set","Has","Window"]
fixedSetHasWindowReactive :: FixedClass self => (self) -> ReactiveFieldWrite IO (Bool)
fixedSetHasWindowReactive w = wrapMW (fixedSetHasWindow w)


-- @C: afterSetScrollAdjustments
-- TODO
-- @A: layoutChildX
-- TODO
-- @A: layoutChildY
-- TODO
-- @G: layoutGetDrawWindow					 | ["layout","Get","Draw","Window"]
layoutGetDrawWindowPassive :: (Layout) -> ReactiveFieldRead IO (DrawWindow)
layoutGetDrawWindowPassive w = wrapMRPassive (layoutGetDrawWindow w)


-- @G: layoutGetHAdjustment					 | ["layout","Get","HAdjustment"]
layoutGetHAdjustmentPassive :: LayoutClass self => (self) -> ReactiveFieldRead IO (Adjustment)
layoutGetHAdjustmentPassive w = wrapMRPassive (layoutGetHAdjustment w)


-- @G: layoutGetSize					 | ["layout","Get","Size"]
layoutGetSizePassive :: LayoutClass self => (self) -> ReactiveFieldRead IO ((Int, Int))
layoutGetSizePassive w = wrapMRPassive (layoutGetSize w)


-- @G: layoutGetVAdjustment					 | ["layout","Get","VAdjustment"]
layoutGetVAdjustmentPassive :: LayoutClass self => (self) -> ReactiveFieldRead IO (Adjustment)
layoutGetVAdjustmentPassive w = wrapMRPassive (layoutGetVAdjustment w)


-- @A: layoutHAdjustment
layoutHAdjustmentPassive :: LayoutClass self => (self) -> ReactiveFieldReadWrite IO (Adjustment)
layoutHAdjustmentPassive w = passivePropertyNE w layoutHAdjustment


-- @A: layoutHeight
layoutHeightPassive :: LayoutClass self => (self) -> ReactiveFieldReadWrite IO (Int)
layoutHeightPassive w = passivePropertyNE w layoutHeight


-- @T: layoutSetHAdjustment					 | ["layout","Set","HAdjustment"]
layoutSetHAdjustmentReactive :: LayoutClass self => (self) -> ReactiveFieldWrite IO (Adjustment)
layoutSetHAdjustmentReactive w = wrapMW (layoutSetHAdjustment w)


-- @T: layoutSetSize					 | ["layout","Set","Size"]
-- TODO
-- @T: layoutSetVAdjustment					 | ["layout","Set","VAdjustment"]
layoutSetVAdjustmentReactive :: LayoutClass self => (self) -> ReactiveFieldWrite IO (Adjustment)
layoutSetVAdjustmentReactive w = wrapMW (layoutSetVAdjustment w)


-- @A: layoutVAdjustment
layoutVAdjustmentPassive :: LayoutClass self => (self) -> ReactiveFieldReadWrite IO (Adjustment)
layoutVAdjustmentPassive w = passivePropertyNE w layoutVAdjustment


-- @A: layoutWidth
layoutWidthPassive :: LayoutClass self => (self) -> ReactiveFieldReadWrite IO (Int)
layoutWidthPassive w = passivePropertyNE w layoutWidth


-- @C: onSetScrollAdjustments
-- TODO
-- @C: afterSwitchPage
-- TODO
-- @A: notebookChildDetachable
notebookChildDetachablePassive :: NotebookClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
notebookChildDetachablePassive w = passivePropertyNE w notebookChildDetachable


-- @A: notebookChildMenuLabel
-- TODO
-- @A: notebookChildPosition
-- TODO
-- @A: notebookChildReorderable
notebookChildReorderablePassive :: NotebookClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
notebookChildReorderablePassive w = passivePropertyNE w notebookChildReorderable


-- @A: notebookChildTabExpand
notebookChildTabExpandPassive :: NotebookClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
notebookChildTabExpandPassive w = passivePropertyNE w notebookChildTabExpand


-- @A: notebookChildTabFill
notebookChildTabFillPassive :: NotebookClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
notebookChildTabFillPassive w = passivePropertyNE w notebookChildTabFill


-- @A: notebookChildTabLabel
-- TODO
-- @A: notebookChildTabPacking
-- TODO
-- @A: notebookChildTabPackType
-- TODO
-- @A: notebookCurrentPage
notebookCurrentPagePassive :: NotebookClass self => (self) -> ReactiveFieldReadWrite IO (Int)
notebookCurrentPagePassive w = passivePropertyNE w notebookCurrentPage


-- @A: notebookEnablePopup
notebookEnablePopupPassive :: NotebookClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
notebookEnablePopupPassive w = passivePropertyNE w notebookEnablePopup


-- @G: notebookGetActionWidget					 | ["notebook","Get","Action","Widget"]
-- TODO
-- @G: notebookGetCurrentPage					 | ["notebook","Get","Current","Page"]
notebookGetCurrentPagePassive :: NotebookClass self => (self) -> ReactiveFieldRead IO (Int)
notebookGetCurrentPagePassive w = wrapMRPassive (notebookGetCurrentPage w)


-- @G: notebookGetMenuLabel					 | ["notebook","Get","Menu","Label"]
-- TODO
-- @G: notebookGetMenuLabelText					 | ["notebook","Get","Menu","Label","Text"]
-- TODO
-- @G: notebookGetNPages					 | ["notebook","Get","NPages"]
notebookGetNPagesPassive :: NotebookClass self => (self) -> ReactiveFieldRead IO (Int)
notebookGetNPagesPassive w = wrapMRPassive (notebookGetNPages w)


-- @G: notebookGetNthPage					 | ["notebook","Get","Nth","Page"]
-- TODO
-- @G: notebookGetScrollable					 | ["notebook","Get","Scrollable"]
notebookGetScrollablePassive :: NotebookClass self => (self) -> ReactiveFieldRead IO (Bool)
notebookGetScrollablePassive w = wrapMRPassive (notebookGetScrollable w)


-- @G: notebookGetShowBorder					 | ["notebook","Get","Show","Border"]
notebookGetShowBorderPassive :: NotebookClass self => (self) -> ReactiveFieldRead IO (Bool)
notebookGetShowBorderPassive w = wrapMRPassive (notebookGetShowBorder w)


-- @G: notebookGetShowTabs					 | ["notebook","Get","Show","Tabs"]
notebookGetShowTabsPassive :: NotebookClass self => (self) -> ReactiveFieldRead IO (Bool)
notebookGetShowTabsPassive w = wrapMRPassive (notebookGetShowTabs w)


-- @G: notebookGetTabDetachable					 | ["notebook","Get","Tab","Detachable"]
-- TODO
-- @G: notebookGetTabLabel					 | ["notebook","Get","Tab","Label"]
-- TODO
-- @G: notebookGetTabLabelText					 | ["notebook","Get","Tab","Label","Text"]
-- TODO
-- @G: notebookGetTabPos					 | ["notebook","Get","Tab","Pos"]
notebookGetTabPosPassive :: NotebookClass self => (self) -> ReactiveFieldRead IO (PositionType)
notebookGetTabPosPassive w = wrapMRPassive (notebookGetTabPos w)


-- @G: notebookGetTabReorderable					 | ["notebook","Get","Tab","Reorderable"]
-- TODO
-- @A: notebookHomogeneous
notebookHomogeneousPassive :: NotebookClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
notebookHomogeneousPassive w = passivePropertyNE w notebookHomogeneous


-- @A: notebookPage
notebookPagePassive :: NotebookClass self => (self) -> ReactiveFieldReadWrite IO (Int)
notebookPagePassive w = passivePropertyNE w notebookPage


-- @A: notebookScrollable
notebookScrollablePassive :: NotebookClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
notebookScrollablePassive w = passivePropertyNE w notebookScrollable


-- @T: notebookSetActionWidget					 | ["notebook","Set","Action","Widget"]
-- TODO
-- @T: notebookSetCurrentPage					 | ["notebook","Set","Current","Page"]
notebookSetCurrentPageReactive :: NotebookClass self => (self) -> ReactiveFieldWrite IO (Int)
notebookSetCurrentPageReactive w = wrapMW (notebookSetCurrentPage w)


-- @T: notebookSetHomogeneousTabs					 | ["notebook","Set","Homogeneous","Tabs"]
notebookSetHomogeneousTabsReactive :: NotebookClass self => (self) -> ReactiveFieldWrite IO (Bool)
notebookSetHomogeneousTabsReactive w = wrapMW (notebookSetHomogeneousTabs w)


-- @T: notebookSetMenuLabel					 | ["notebook","Set","Menu","Label"]
-- TODO
-- @T: notebookSetMenuLabelText					 | ["notebook","Set","Menu","Label","Text"]
-- TODO
-- @T: notebookSetPopup					 | ["notebook","Set","Popup"]
notebookSetPopupReactive :: NotebookClass self => (self) -> ReactiveFieldWrite IO (Bool)
notebookSetPopupReactive w = wrapMW (notebookSetPopup w)


-- @T: notebookSetScrollable					 | ["notebook","Set","Scrollable"]
notebookSetScrollableReactive :: NotebookClass self => (self) -> ReactiveFieldWrite IO (Bool)
notebookSetScrollableReactive w = wrapMW (notebookSetScrollable w)


-- @T: notebookSetShowBorder					 | ["notebook","Set","Show","Border"]
notebookSetShowBorderReactive :: NotebookClass self => (self) -> ReactiveFieldWrite IO (Bool)
notebookSetShowBorderReactive w = wrapMW (notebookSetShowBorder w)


-- @T: notebookSetShowTabs					 | ["notebook","Set","Show","Tabs"]
notebookSetShowTabsReactive :: NotebookClass self => (self) -> ReactiveFieldWrite IO (Bool)
notebookSetShowTabsReactive w = wrapMW (notebookSetShowTabs w)


-- @T: notebookSetTabBorder					 | ["notebook","Set","Tab","Border"]
notebookSetTabBorderReactive :: NotebookClass self => (self) -> ReactiveFieldWrite IO (Int)
notebookSetTabBorderReactive w = wrapMW (notebookSetTabBorder w)


-- @T: notebookSetTabDetachable					 | ["notebook","Set","Tab","Detachable"]
-- TODO
-- @T: notebookSetTabHBorder					 | ["notebook","Set","Tab","HBorder"]
notebookSetTabHBorderReactive :: NotebookClass self => (self) -> ReactiveFieldWrite IO (Int)
notebookSetTabHBorderReactive w = wrapMW (notebookSetTabHBorder w)


-- @T: notebookSetTabLabel					 | ["notebook","Set","Tab","Label"]
-- TODO
-- @T: notebookSetTabLabelPacking					 | ["notebook","Set","Tab","Label","Packing"]
-- TODO
-- @T: notebookSetTabLabelText					 | ["notebook","Set","Tab","Label","Text"]
-- TODO
-- @T: notebookSetTabPos					 | ["notebook","Set","Tab","Pos"]
notebookSetTabPosReactive :: NotebookClass self => (self) -> ReactiveFieldWrite IO (PositionType)
notebookSetTabPosReactive w = wrapMW (notebookSetTabPos w)


-- @T: notebookSetTabReorderable					 | ["notebook","Set","Tab","Reorderable"]
-- TODO
-- @T: notebookSetTabVBorder					 | ["notebook","Set","Tab","VBorder"]
notebookSetTabVBorderReactive :: NotebookClass self => (self) -> ReactiveFieldWrite IO (Int)
notebookSetTabVBorderReactive w = wrapMW (notebookSetTabVBorder w)


-- @A: notebookShowBorder
notebookShowBorderPassive :: NotebookClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
notebookShowBorderPassive w = passivePropertyNE w notebookShowBorder


-- @A: notebookShowTabs
notebookShowTabsPassive :: NotebookClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
notebookShowTabsPassive w = passivePropertyNE w notebookShowTabs


-- @A: notebookTabHborder
notebookTabHborderPassive :: NotebookClass self => (self) -> ReactiveFieldReadWrite IO (Int)
notebookTabHborderPassive w = passivePropertyNE w notebookTabHborder


-- @A: notebookTabPos
notebookTabPosPassive :: NotebookClass self => (self) -> ReactiveFieldReadWrite IO (PositionType)
notebookTabPosPassive w = passivePropertyNE w notebookTabPos


-- @A: notebookTabVborder
notebookTabVborderPassive :: NotebookClass self => (self) -> ReactiveFieldReadWrite IO (Int)
notebookTabVborderPassive w = passivePropertyNE w notebookTabVborder


-- @C: onSwitchPage
-- TODO
-- @S: pageAdded
-- TODO
-- @S: pageRemoved
-- TODO
-- @S: pageReordered
-- TODO
-- @S: switchPage
-- TODO
-- @A: tableChildBottomAttach
-- TODO
-- @A: tableChildLeftAttach
-- TODO
-- @A: tableChildRightAttach
-- TODO
-- @A: tableChildTopAttach
-- TODO
-- @A: tableChildXOptions
-- TODO
-- @A: tableChildXPadding
-- TODO
-- @A: tableChildYOptions
-- TODO
-- @A: tableChildYPadding
-- TODO
-- @A: tableColumnSpacing
tableColumnSpacingPassive :: TableClass self => (self) -> ReactiveFieldReadWrite IO (Int)
tableColumnSpacingPassive w = passivePropertyNE w tableColumnSpacing


-- @G: tableGetColSpacing					 | ["table","Get","Col","Spacing"]
-- TODO
-- @G: tableGetDefaultColSpacing					 | ["table","Get","Default","Col","Spacing"]
tableGetDefaultColSpacingPassive :: TableClass self => (self) -> ReactiveFieldRead IO (Int)
tableGetDefaultColSpacingPassive w = wrapMRPassive (tableGetDefaultColSpacing w)


-- @G: tableGetDefaultRowSpacing					 | ["table","Get","Default","Row","Spacing"]
tableGetDefaultRowSpacingPassive :: TableClass self => (self) -> ReactiveFieldRead IO (Int)
tableGetDefaultRowSpacingPassive w = wrapMRPassive (tableGetDefaultRowSpacing w)


-- @G: tableGetHomogeneous					 | ["table","Get","Homogeneous"]
tableGetHomogeneousPassive :: TableClass self => (self) -> ReactiveFieldRead IO (Bool)
tableGetHomogeneousPassive w = wrapMRPassive (tableGetHomogeneous w)


-- @G: tableGetRowSpacing					 | ["table","Get","Row","Spacing"]
-- TODO
-- @G: tableGetSize					 | ["table","Get","Size"]
tableGetSizePassive :: TableClass self => (self) -> ReactiveFieldRead IO ((Int, Int))
tableGetSizePassive w = wrapMRPassive (tableGetSize w)


-- @A: tableHomogeneous
tableHomogeneousPassive :: TableClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
tableHomogeneousPassive w = passivePropertyNE w tableHomogeneous


-- @A: tableNColumns
tableNColumnsPassive :: TableClass self => (self) -> ReactiveFieldReadWrite IO (Int)
tableNColumnsPassive w = passivePropertyNE w tableNColumns


-- @A: tableNRows
tableNRowsPassive :: TableClass self => (self) -> ReactiveFieldReadWrite IO (Int)
tableNRowsPassive w = passivePropertyNE w tableNRows


-- @A: tableRowSpacing
tableRowSpacingPassive :: TableClass self => (self) -> ReactiveFieldReadWrite IO (Int)
tableRowSpacingPassive w = passivePropertyNE w tableRowSpacing


-- @T: tableSetColSpacing					 | ["table","Set","Col","Spacing"]
-- TODO
-- @T: tableSetColSpacings					 | ["table","Set","Col","Spacings"]
tableSetColSpacingsReactive :: TableClass self => (self) -> ReactiveFieldWrite IO (Int)
tableSetColSpacingsReactive w = wrapMW (tableSetColSpacings w)


-- @T: tableSetHomogeneous					 | ["table","Set","Homogeneous"]
tableSetHomogeneousReactive :: TableClass self => (self) -> ReactiveFieldWrite IO (Bool)
tableSetHomogeneousReactive w = wrapMW (tableSetHomogeneous w)


-- @T: tableSetRowSpacing					 | ["table","Set","Row","Spacing"]
-- TODO
-- @T: tableSetRowSpacings					 | ["table","Set","Row","Spacings"]
tableSetRowSpacingsReactive :: TableClass self => (self) -> ReactiveFieldWrite IO (Int)
tableSetRowSpacingsReactive w = wrapMW (tableSetRowSpacings w)


-- @A: checkMenuItemActive
checkMenuItemActivePassive :: CheckMenuItemClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
checkMenuItemActivePassive w = passivePropertyNE w checkMenuItemActive


-- @A: checkMenuItemDrawAsRadio
checkMenuItemDrawAsRadioPassive :: CheckMenuItemClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
checkMenuItemDrawAsRadioPassive w = passivePropertyNE w checkMenuItemDrawAsRadio


-- @G: checkMenuItemGetActive					 | ["check","Menu","Item","Get","Active"]
checkMenuItemGetActivePassive :: CheckMenuItemClass self => (self) -> ReactiveFieldRead IO (Bool)
checkMenuItemGetActivePassive w = wrapMRPassive (checkMenuItemGetActive w)


-- @G: checkMenuItemGetDrawAsRadio					 | ["check","Menu","Item","Get","Draw","As","Radio"]
checkMenuItemGetDrawAsRadioPassive :: CheckMenuItemClass self => (self) -> ReactiveFieldRead IO (Bool)
checkMenuItemGetDrawAsRadioPassive w = wrapMRPassive (checkMenuItemGetDrawAsRadio w)


-- @G: checkMenuItemGetInconsistent					 | ["check","Menu","Item","Get","Inconsistent"]
checkMenuItemGetInconsistentPassive :: CheckMenuItemClass self => (self) -> ReactiveFieldRead IO (Bool)
checkMenuItemGetInconsistentPassive w = wrapMRPassive (checkMenuItemGetInconsistent w)


-- @A: checkMenuItemInconsistent
checkMenuItemInconsistentPassive :: CheckMenuItemClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
checkMenuItemInconsistentPassive w = passivePropertyNE w checkMenuItemInconsistent


-- @T: checkMenuItemSetActive					 | ["check","Menu","Item","Set","Active"]
checkMenuItemSetActiveReactive :: CheckMenuItemClass self => (self) -> ReactiveFieldWrite IO (Bool)
checkMenuItemSetActiveReactive w = wrapMW (checkMenuItemSetActive w)


-- @T: checkMenuItemSetDrawAsRadio					 | ["check","Menu","Item","Set","Draw","As","Radio"]
checkMenuItemSetDrawAsRadioReactive :: CheckMenuItemClass self => (self) -> ReactiveFieldWrite IO (Bool)
checkMenuItemSetDrawAsRadioReactive w = wrapMW (checkMenuItemSetDrawAsRadio w)


-- @T: checkMenuItemSetInconsistent					 | ["check","Menu","Item","Set","Inconsistent"]
checkMenuItemSetInconsistentReactive :: CheckMenuItemClass self => (self) -> ReactiveFieldWrite IO (Bool)
checkMenuItemSetInconsistentReactive w = wrapMW (checkMenuItemSetInconsistent w)


-- @S: checkMenuItemToggled
checkMenuItemToggledReactive :: CheckMenuItemClass self => self -> ReactiveFieldRead IO ()
checkMenuItemToggledReactive = (`reactiveSignalIO` checkMenuItemToggled)


-- @C: afterChanged
afterChangedReactive :: ComboBoxClass self => self -> ReactiveFieldRead IO ()
afterChangedReactive w = reactivePropertyH_ w afterChanged

-- @S: changed
changedReactive :: ComboBoxClass self => self -> ReactiveFieldRead IO ()
changedReactive = (`reactiveSignalIO` changed)


-- @A: comboBoxActive
comboBoxActivePassive :: ComboBoxClass self => (self) -> ReactiveFieldReadWrite IO (Int)
comboBoxActivePassive w = passivePropertyNE w comboBoxActive


-- @A: comboBoxAddTearoffs
comboBoxAddTearoffsPassive :: ComboBoxClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
comboBoxAddTearoffsPassive w = passivePropertyNE w comboBoxAddTearoffs


-- @A: comboBoxColumnSpanColumn
comboBoxColumnSpanColumnPassive :: ComboBoxClass self => (self) -> ReactiveFieldReadWrite IO ((ColumnId row Int))
comboBoxColumnSpanColumnPassive w = passivePropertyNE w comboBoxColumnSpanColumn


-- @A: comboBoxFocusOnClick
comboBoxFocusOnClickPassive :: ComboBoxClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
comboBoxFocusOnClickPassive w = passivePropertyNE w comboBoxFocusOnClick


-- @G: comboBoxGetActive					 | ["combo","Box","Get","Active"]
comboBoxGetActivePassive :: ComboBoxClass self => (self) -> ReactiveFieldRead IO (Int)
comboBoxGetActivePassive w = wrapMRPassive (comboBoxGetActive w)


-- @G: comboBoxGetActiveIter					 | ["combo","Box","Get","Active","Iter"]
comboBoxGetActiveIterPassive :: ComboBoxClass self => (self) -> ReactiveFieldRead IO ((Maybe TreeIter))
comboBoxGetActiveIterPassive w = wrapMRPassive (comboBoxGetActiveIter w)


-- @G: comboBoxGetActiveText					 | ["combo","Box","Get","Active","Text"]
comboBoxGetActiveTextPassive :: ComboBoxClass self => (self) -> ReactiveFieldRead IO ((Maybe ComboBoxText))
comboBoxGetActiveTextPassive w = wrapMRPassive (comboBoxGetActiveText w)


-- @G: comboBoxGetAddTearoffs					 | ["combo","Box","Get","Add","Tearoffs"]
comboBoxGetAddTearoffsPassive :: ComboBoxClass self => (self) -> ReactiveFieldRead IO (Bool)
comboBoxGetAddTearoffsPassive w = wrapMRPassive (comboBoxGetAddTearoffs w)


-- @G: comboBoxGetColumnSpanColumn					 | ["combo","Box","Get","Column","Span","Column"]
comboBoxGetColumnSpanColumnPassive :: ComboBoxClass self => (self) -> ReactiveFieldRead IO ((ColumnId row Int))
comboBoxGetColumnSpanColumnPassive w = wrapMRPassive (comboBoxGetColumnSpanColumn w)


-- @G: comboBoxGetEntryTextColumn					 | ["combo","Box","Get","Entry","Text","Column"]
comboBoxGetEntryTextColumnPassive :: ComboBoxClass comboBox => (comboBox) -> ReactiveFieldRead IO ((ColumnId row ComboBoxText))
comboBoxGetEntryTextColumnPassive w = wrapMRPassive (comboBoxGetEntryTextColumn w)


-- @G: comboBoxGetFocusOnClick					 | ["combo","Box","Get","Focus","On","Click"]
comboBoxGetFocusOnClickPassive :: ComboBoxClass self => (self) -> ReactiveFieldRead IO (Bool)
comboBoxGetFocusOnClickPassive w = wrapMRPassive (comboBoxGetFocusOnClick w)


-- @G: comboBoxGetHasEntry					 | ["combo","Box","Get","Has","Entry"]
comboBoxGetHasEntryPassive :: ComboBoxClass self => (self) -> ReactiveFieldRead IO (Bool)
comboBoxGetHasEntryPassive w = wrapMRPassive (comboBoxGetHasEntry w)


-- @G: comboBoxGetModel					 | ["combo","Box","Get","Model"]
comboBoxGetModelPassive :: ComboBoxClass self => (self) -> ReactiveFieldRead IO ((Maybe TreeModel))
comboBoxGetModelPassive w = wrapMRPassive (comboBoxGetModel w)


-- @G: comboBoxGetModelText					 | ["combo","Box","Get","Model","Text"]
comboBoxGetModelTextPassive :: ComboBoxClass self => (self) -> ReactiveFieldRead IO ((ListStore ComboBoxText))
comboBoxGetModelTextPassive w = wrapMRPassive (comboBoxGetModelText w)


-- @G: comboBoxGetRowSpanColumn					 | ["combo","Box","Get","Row","Span","Column"]
comboBoxGetRowSpanColumnPassive :: ComboBoxClass self => (self) -> ReactiveFieldRead IO ((ColumnId row Int))
comboBoxGetRowSpanColumnPassive w = wrapMRPassive (comboBoxGetRowSpanColumn w)


-- @G: comboBoxGetTitle					 | ["combo","Box","Get","Title"]
comboBoxGetTitlePassive :: (ComboBoxClass self, GlibString string) => (self) -> ReactiveFieldRead IO (string)
comboBoxGetTitlePassive w = wrapMRPassive (comboBoxGetTitle w)


-- @G: comboBoxGetWrapWidth					 | ["combo","Box","Get","Wrap","Width"]
comboBoxGetWrapWidthPassive :: ComboBoxClass self => (self) -> ReactiveFieldRead IO (Int)
comboBoxGetWrapWidthPassive w = wrapMRPassive (comboBoxGetWrapWidth w)


-- @A: comboBoxHasFrame
comboBoxHasFramePassive :: ComboBoxClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
comboBoxHasFramePassive w = passivePropertyNE w comboBoxHasFrame


-- @A: comboBoxRowSpanColumn
comboBoxRowSpanColumnPassive :: ComboBoxClass self => (self) -> ReactiveFieldReadWrite IO ((ColumnId row Int))
comboBoxRowSpanColumnPassive w = passivePropertyNE w comboBoxRowSpanColumn


-- @T: comboBoxSetActive					 | ["combo","Box","Set","Active"]
comboBoxSetActiveReactive :: ComboBoxClass self => (self) -> ReactiveFieldWrite IO (Int)
comboBoxSetActiveReactive w = wrapMW (comboBoxSetActive w)


-- @T: comboBoxSetActiveIter					 | ["combo","Box","Set","Active","Iter"]
comboBoxSetActiveIterReactive :: ComboBoxClass self => (self) -> ReactiveFieldWrite IO (TreeIter)
comboBoxSetActiveIterReactive w = wrapMW (comboBoxSetActiveIter w)


-- @T: comboBoxSetAddTearoffs					 | ["combo","Box","Set","Add","Tearoffs"]
comboBoxSetAddTearoffsReactive :: ComboBoxClass self => (self) -> ReactiveFieldWrite IO (Bool)
comboBoxSetAddTearoffsReactive w = wrapMW (comboBoxSetAddTearoffs w)


-- @T: comboBoxSetColumnSpanColumn					 | ["combo","Box","Set","Column","Span","Column"]
comboBoxSetColumnSpanColumnReactive :: ComboBoxClass self => (self) -> ReactiveFieldWrite IO (ColumnId row Int)
comboBoxSetColumnSpanColumnReactive w = wrapMW (comboBoxSetColumnSpanColumn w)


-- @T: comboBoxSetEntryTextColumn					 | ["combo","Box","Set","Entry","Text","Column"]
comboBoxSetEntryTextColumnReactive :: ComboBoxClass comboBox => (comboBox) -> ReactiveFieldWrite IO (ColumnId row ComboBoxText)
comboBoxSetEntryTextColumnReactive w = wrapMW (comboBoxSetEntryTextColumn w)


-- @T: comboBoxSetFocusOnClick					 | ["combo","Box","Set","Focus","On","Click"]
comboBoxSetFocusOnClickReactive :: ComboBoxClass self => (self) -> ReactiveFieldWrite IO (Bool)
comboBoxSetFocusOnClickReactive w = wrapMW (comboBoxSetFocusOnClick w)


-- @T: comboBoxSetModel					 | ["combo","Box","Set","Model"]
comboBoxSetModelReactive :: (ComboBoxClass self, TreeModelClass model) => (self) -> ReactiveFieldWrite IO (Maybe model)
comboBoxSetModelReactive w = wrapMW (comboBoxSetModel w)


-- @T: comboBoxSetModelText					 | ["combo","Box","Set","Model","Text"]
-- TODO
-- @T: comboBoxSetRowSpanColumn					 | ["combo","Box","Set","Row","Span","Column"]
comboBoxSetRowSpanColumnReactive :: ComboBoxClass self => (self) -> ReactiveFieldWrite IO (ColumnId row Int)
comboBoxSetRowSpanColumnReactive w = wrapMW (comboBoxSetRowSpanColumn w)


-- @T: comboBoxSetTitle					 | ["combo","Box","Set","Title"]
comboBoxSetTitleReactive :: (ComboBoxClass self, GlibString string) => (self) -> ReactiveFieldWrite IO (string)
comboBoxSetTitleReactive w = wrapMW (comboBoxSetTitle w)


-- @T: comboBoxSetWrapWidth					 | ["combo","Box","Set","Wrap","Width"]
comboBoxSetWrapWidthReactive :: ComboBoxClass self => (self) -> ReactiveFieldWrite IO (Int)
comboBoxSetWrapWidthReactive w = wrapMW (comboBoxSetWrapWidth w)


-- @A: comboBoxTearoffTitle
comboBoxTearoffTitlePassive :: (ComboBoxClass self, GlibString string) => (self) -> ReactiveFieldReadWrite IO (string)
comboBoxTearoffTitlePassive w = passivePropertyNE w comboBoxTearoffTitle


-- @A: comboBoxTitle
comboBoxTitlePassive :: (ComboBoxClass self, GlibString string) => (self) -> ReactiveFieldReadWrite IO (string)
comboBoxTitlePassive w = passivePropertyNE w comboBoxTitle


-- @A: comboBoxWrapWidth
comboBoxWrapWidthPassive :: ComboBoxClass self => (self) -> ReactiveFieldReadWrite IO (Int)
comboBoxWrapWidthPassive w = passivePropertyNE w comboBoxWrapWidth


-- @G: comboBoxEntryGetActiveText					 | ["combo","Box","Entry","Get","Active","Text"]
comboBoxEntryGetActiveTextPassive :: (ComboBoxEntryClass self, GlibString string) => (self) -> ReactiveFieldRead IO ((Maybe string))
comboBoxEntryGetActiveTextPassive w = wrapMRPassive (comboBoxEntryGetActiveText w)


-- @G: comboBoxEntryGetTextColumn					 | ["combo","Box","Entry","Get","Text","Column"]
comboBoxEntryGetTextColumnPassive :: (ComboBoxEntryClass self, GlibString string) => (self) -> ReactiveFieldRead IO ((ColumnId row string))
comboBoxEntryGetTextColumnPassive w = wrapMRPassive (comboBoxEntryGetTextColumn w)


-- @T: comboBoxEntrySetModelText					 | ["combo","Box","Entry","Set","Model","Text"]
-- TODO
-- @T: comboBoxEntrySetTextColumn					 | ["combo","Box","Entry","Set","Text","Column"]
comboBoxEntrySetTextColumnReactive :: (ComboBoxEntryClass self, GlibString string) => (self) -> ReactiveFieldWrite IO (ColumnId row string)
comboBoxEntrySetTextColumnReactive w = wrapMW (comboBoxEntrySetTextColumn w)


-- @A: comboBoxEntryTextColumn
comboBoxEntryTextColumnPassive :: (ComboBoxEntryClass self, GlibString string) => (self) -> ReactiveFieldReadWrite IO ((ColumnId row string))
comboBoxEntryTextColumnPassive w = passivePropertyNE w comboBoxEntryTextColumn


-- @C: onChanged
onChangedReactive :: ComboBoxClass self => self -> ReactiveFieldRead IO ()
onChangedReactive w = reactivePropertyH_ w onChanged

-- @A: comboAllowEmpty
comboAllowEmptyPassive :: ComboClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
comboAllowEmptyPassive w = passivePropertyNE w comboAllowEmpty


-- @A: comboCaseSensitive
comboCaseSensitivePassive :: ComboClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
comboCaseSensitivePassive w = passivePropertyNE w comboCaseSensitive


-- @A: comboEnableArrowKeys
comboEnableArrowKeysPassive :: ComboClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
comboEnableArrowKeysPassive w = passivePropertyNE w comboEnableArrowKeys


-- @A: comboEnableArrowsAlways
comboEnableArrowsAlwaysPassive :: ComboClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
comboEnableArrowsAlwaysPassive w = passivePropertyNE w comboEnableArrowsAlways


-- @T: comboSetCaseSensitive					 | ["combo","Set","Case","Sensitive"]
comboSetCaseSensitiveReactive :: ComboClass self => (self) -> ReactiveFieldWrite IO (Bool)
comboSetCaseSensitiveReactive w = wrapMW (comboSetCaseSensitive w)


-- @T: comboSetPopdownStrings					 | ["combo","Set","Popdown","Strings"]
comboSetPopdownStringsReactive :: (ComboClass self, GlibString string) => (self) -> ReactiveFieldWrite IO ([string])
comboSetPopdownStringsReactive w = wrapMW (comboSetPopdownStrings w)


-- @T: comboSetUseArrowsAlways					 | ["combo","Set","Use","Arrows","Always"]
comboSetUseArrowsAlwaysReactive :: ComboClass self => (self) -> ReactiveFieldWrite IO (Bool)
comboSetUseArrowsAlwaysReactive w = wrapMW (comboSetUseArrowsAlways w)


-- @T: comboSetUseArrows					 | ["combo","Set","Use","Arrows"]
comboSetUseArrowsReactive :: ComboClass self => (self) -> ReactiveFieldWrite IO (Bool)
comboSetUseArrowsReactive w = wrapMW (comboSetUseArrows w)


-- @T: comboSetValueInList					 | ["combo","Set","Value","In","List"]
-- TODO
-- @A: comboValueInList
comboValueInListPassive :: ComboClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
comboValueInListPassive w = passivePropertyNE w comboValueInList


-- @G: imageMenuItemGetImage					 | ["image","Menu","Item","Get","Image"]
imageMenuItemGetImagePassive :: ImageMenuItemClass self => (self) -> ReactiveFieldRead IO ((Maybe Widget))
imageMenuItemGetImagePassive w = wrapMRPassive (imageMenuItemGetImage w)


-- @T: imageMenuItemSetImage					 | ["image","Menu","Item","Set","Image"]
imageMenuItemSetImageReactive :: (ImageMenuItemClass self, WidgetClass image) => (self) -> ReactiveFieldWrite IO (image)
imageMenuItemSetImageReactive w = wrapMW (imageMenuItemSetImage w)


-- @A: menuBarChildPackDirection
menuBarChildPackDirectionPassive :: MenuBarClass self => (self) -> ReactiveFieldReadWrite IO (PackDirection)
menuBarChildPackDirectionPassive w = passivePropertyNE w menuBarChildPackDirection


-- @G: menuBarGetChildPackDirection					 | ["menu","Bar","Get","Child","Pack","Direction"]
menuBarGetChildPackDirectionPassive :: MenuBarClass self => (self) -> ReactiveFieldRead IO (PackDirection)
menuBarGetChildPackDirectionPassive w = wrapMRPassive (menuBarGetChildPackDirection w)


-- @G: menuBarGetPackDirection					 | ["menu","Bar","Get","Pack","Direction"]
menuBarGetPackDirectionPassive :: MenuBarClass self => (self) -> ReactiveFieldRead IO (PackDirection)
menuBarGetPackDirectionPassive w = wrapMRPassive (menuBarGetPackDirection w)


-- @A: menuBarPackDirection
menuBarPackDirectionPassive :: MenuBarClass self => (self) -> ReactiveFieldReadWrite IO (PackDirection)
menuBarPackDirectionPassive w = passivePropertyNE w menuBarPackDirection


-- @T: menuBarSetChildPackDirection					 | ["menu","Bar","Set","Child","Pack","Direction"]
menuBarSetChildPackDirectionReactive :: MenuBarClass self => (self) -> ReactiveFieldWrite IO (PackDirection)
menuBarSetChildPackDirectionReactive w = wrapMW (menuBarSetChildPackDirection w)


-- @T: menuBarSetPackDirection					 | ["menu","Bar","Set","Pack","Direction"]
menuBarSetPackDirectionReactive :: MenuBarClass self => (self) -> ReactiveFieldWrite IO (PackDirection)
menuBarSetPackDirectionReactive w = wrapMW (menuBarSetPackDirection w)


-- @C: afterActivateItem
afterActivateItemReactive :: MenuItemClass self => self -> ReactiveFieldRead IO ()
afterActivateItemReactive w = reactivePropertyH_ w afterActivateItem

-- @C: afterActivateLeaf
afterActivateLeafReactive :: MenuItemClass self => self -> ReactiveFieldRead IO ()
afterActivateLeafReactive w = reactivePropertyH_ w afterActivateLeaf

-- @C: afterDeselect

afterDeselectReactive w = reactivePropertyH_ w afterDeselect

-- @C: afterSelect

afterSelectReactive w = reactivePropertyH_ w afterSelect

-- @C: afterToggle

afterToggleReactive w = reactivePropertyH_ w afterToggle

-- @S: menuItemActivated
menuItemActivatedReactive :: MenuItemClass self => self -> ReactiveFieldRead IO ()
menuItemActivatedReactive = (`reactiveSignalIO` menuItemActivated)


-- @S: menuItemActivatedItem
menuItemActivatedItemReactive :: MenuItemClass self => self -> ReactiveFieldRead IO ()
menuItemActivatedItemReactive = (`reactiveSignalIO` menuItemActivatedItem)


-- @S: menuItemActivate
menuItemActivateReactive :: MenuItemClass self => self -> ReactiveFieldRead IO ()
menuItemActivateReactive = (`reactiveSignalIO` menuItemActivate)


-- @S: menuItemActivateItem
menuItemActivateItemReactive :: MenuItemClass self => self -> ReactiveFieldRead IO ()
menuItemActivateItemReactive = (`reactiveSignalIO` menuItemActivateItem)


-- @S: menuItemDeselect
menuItemDeselectReactive :: MenuItemClass self => self -> ReactiveFieldRead IO ()
menuItemDeselectReactive = (`reactiveSignalIO` menuItemDeselect)


-- @G: menuItemGetLabel					 | ["menu","Item","Get","Label"]
menuItemGetLabelPassive :: (MenuItemClass self, GlibString string) => (self) -> ReactiveFieldRead IO (string)
menuItemGetLabelPassive w = wrapMRPassive (menuItemGetLabel w)


-- @G: menuItemGetRightJustified					 | ["menu","Item","Get","Right","Justified"]
menuItemGetRightJustifiedPassive :: MenuItemClass self => (self) -> ReactiveFieldRead IO (Bool)
menuItemGetRightJustifiedPassive w = wrapMRPassive (menuItemGetRightJustified w)


-- @G: menuItemGetSubmenu					 | ["menu","Item","Get","Submenu"]
menuItemGetSubmenuPassive :: MenuItemClass self => (self) -> ReactiveFieldRead IO ((Maybe Widget))
menuItemGetSubmenuPassive w = wrapMRPassive (menuItemGetSubmenu w)


-- @G: menuItemGetUseUnderline					 | ["menu","Item","Get","Use","Underline"]
menuItemGetUseUnderlinePassive :: MenuItemClass self => (self) -> ReactiveFieldRead IO (Bool)
menuItemGetUseUnderlinePassive w = wrapMRPassive (menuItemGetUseUnderline w)


-- @A: menuItemLabel
menuItemLabelPassive :: (MenuItemClass self, GlibString string) => (self) -> ReactiveFieldReadWrite IO (string)
menuItemLabelPassive w = passivePropertyNE w menuItemLabel


-- @A: menuItemRightJustified
menuItemRightJustifiedPassive :: MenuItemClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
menuItemRightJustifiedPassive w = passivePropertyNE w menuItemRightJustified


-- @S: menuItemSelect
menuItemSelectReactive :: MenuItemClass self => self -> ReactiveFieldRead IO ()
menuItemSelectReactive = (`reactiveSignalIO` menuItemSelect)


-- @T: menuItemSetAccelPath					 | ["menu","Item","Set","Accel","Path"]
menuItemSetAccelPathReactive :: (MenuItemClass self, GlibString string) => (self) -> ReactiveFieldWrite IO (Maybe string)
menuItemSetAccelPathReactive w = wrapMW (menuItemSetAccelPath w)


-- @T: menuItemSetLabel					 | ["menu","Item","Set","Label"]
menuItemSetLabelReactive :: (MenuItemClass self, GlibString string) => (self) -> ReactiveFieldWrite IO (string)
menuItemSetLabelReactive w = wrapMW (menuItemSetLabel w)


-- @T: menuItemSetRightJustified					 | ["menu","Item","Set","Right","Justified"]
menuItemSetRightJustifiedReactive :: MenuItemClass self => (self) -> ReactiveFieldWrite IO (Bool)
menuItemSetRightJustifiedReactive w = wrapMW (menuItemSetRightJustified w)


-- @T: menuItemSetSubmenu					 | ["menu","Item","Set","Submenu"]
menuItemSetSubmenuReactive :: (MenuItemClass self, MenuClass submenu) => (self) -> ReactiveFieldWrite IO (submenu)
menuItemSetSubmenuReactive w = wrapMW (menuItemSetSubmenu w)


-- @T: menuItemSetUseUnderline					 | ["menu","Item","Set","Use","Underline"]
menuItemSetUseUnderlineReactive :: MenuItemClass self => (self) -> ReactiveFieldWrite IO (Bool)
menuItemSetUseUnderlineReactive w = wrapMW (menuItemSetUseUnderline w)


-- @S: menuItemToggle
menuItemToggleReactive :: MenuItemClass self => self -> ReactiveFieldRead IO ()
menuItemToggleReactive = (`reactiveSignalIO` menuItemToggle)


-- @A: menuItemUseUnderline
menuItemUseUnderlinePassive :: MenuItemClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
menuItemUseUnderlinePassive w = passivePropertyNE w menuItemUseUnderline


-- @C: onActivateItem
onActivateItemReactive :: MenuItemClass self => self -> ReactiveFieldRead IO ()
onActivateItemReactive w = reactivePropertyH_ w onActivateItem

-- @C: onActivateLeaf
onActivateLeafReactive :: MenuItemClass self => self -> ReactiveFieldRead IO ()
onActivateLeafReactive w = reactivePropertyH_ w onActivateLeaf

-- @C: onDeselect

onDeselectReactive w = reactivePropertyH_ w onDeselect

-- @C: onSelect

onSelectReactive w = reactivePropertyH_ w onSelect

-- @C: onToggle

onToggleReactive w = reactivePropertyH_ w onToggle

-- @A: menuAccelGroup

menuAccelGroupPassive w = passivePropertyNE w menuAccelGroup


-- @A: menuChildBottomAttach
-- TODO
-- @A: menuChildLeftAttach
-- TODO
-- @A: menuChildRightAttach
-- TODO
-- @A: menuChildTopAttach
-- TODO
-- @G: menuGetAccelGroup					 | ["menu","Get","Accel","Group"]

menuGetAccelGroupPassive w = wrapMRPassive (menuGetAccelGroup w)


-- @G: menuGetActive					 | ["menu","Get","Active"]
menuGetActivePassive :: MenuClass self => (self) -> ReactiveFieldRead IO (MenuItem)
menuGetActivePassive w = wrapMRPassive (menuGetActive w)


-- @G: menuGetAttachWidget					 | ["menu","Get","Attach","Widget"]
menuGetAttachWidgetPassive :: MenuClass self => (self) -> ReactiveFieldRead IO ((Maybe Widget))
menuGetAttachWidgetPassive w = wrapMRPassive (menuGetAttachWidget w)


-- @G: menuGetForAttachWidget					 | ["menu","Get","For","Attach","Widget"]
menuGetForAttachWidgetPassive :: WidgetClass widget => (widget) -> ReactiveFieldRead IO ([Menu])
menuGetForAttachWidgetPassive w = wrapMRPassive (menuGetForAttachWidget w)


-- @G: menuGetTearoffState					 | ["menu","Get","Tearoff","State"]
menuGetTearoffStatePassive :: MenuClass self => (self) -> ReactiveFieldRead IO (Bool)
menuGetTearoffStatePassive w = wrapMRPassive (menuGetTearoffState w)


-- @G: menuGetTitle					 | ["menu","Get","Title"]
menuGetTitlePassive :: (MenuClass self, GlibString string) => (self) -> ReactiveFieldRead IO ((Maybe string))
menuGetTitlePassive w = wrapMRPassive (menuGetTitle w)


-- @T: menuSetAccelGroup					 | ["menu","Set","Accel","Group"]

menuSetAccelGroupReactive w = wrapMW (menuSetAccelGroup w)


-- @T: menuSetAccelPath					 | ["menu","Set","Accel","Path"]
menuSetAccelPathReactive :: (MenuClass self, GlibString string) => (self) -> ReactiveFieldWrite IO (string)
menuSetAccelPathReactive w = wrapMW (menuSetAccelPath w)


-- @T: menuSetActive					 | ["menu","Set","Active"]
menuSetActiveReactive :: MenuClass self => (self) -> ReactiveFieldWrite IO (Int)
menuSetActiveReactive w = wrapMW (menuSetActive w)


-- @T: menuSetMonitor					 | ["menu","Set","Monitor"]
menuSetMonitorReactive :: MenuClass self => (self) -> ReactiveFieldWrite IO (Int)
menuSetMonitorReactive w = wrapMW (menuSetMonitor w)


-- @T: menuSetScreen					 | ["menu","Set","Screen"]
menuSetScreenReactive :: MenuClass self => (self) -> ReactiveFieldWrite IO (Maybe Screen)
menuSetScreenReactive w = wrapMW (menuSetScreen w)


-- @T: menuSetTearoffState					 | ["menu","Set","Tearoff","State"]
menuSetTearoffStateReactive :: MenuClass self => (self) -> ReactiveFieldWrite IO (Bool)
menuSetTearoffStateReactive w = wrapMW (menuSetTearoffState w)


-- @T: menuSetTitle					 | ["menu","Set","Title"]
menuSetTitleReactive :: (MenuClass self, GlibString string) => (self) -> ReactiveFieldWrite IO (string)
menuSetTitleReactive w = wrapMW (menuSetTitle w)


-- @A: menuTearoffState
menuTearoffStatePassive :: MenuClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
menuTearoffStatePassive w = passivePropertyNE w menuTearoffState


-- @A: menuTitle
menuTitlePassive :: (MenuClass self, GlibString string) => (self) -> ReactiveFieldReadWrite IO (string)
menuTitlePassive w = passivePropertyNE w menuTitle


-- @C: afterActivateCurrent
-- TODO
-- @C: afterCancel
afterCancelReactive :: MenuShellClass self => self -> ReactiveFieldRead IO ()
afterCancelReactive w = reactivePropertyH_ w afterCancel

-- @C: afterDeactivated
afterDeactivatedReactive :: MenuShellClass self => self -> ReactiveFieldRead IO ()
afterDeactivatedReactive w = reactivePropertyH_ w afterDeactivated

-- @C: afterMoveCurrent
-- TODO
-- @C: afterSelectionDone
afterSelectionDoneReactive :: MenuShellClass self => self -> ReactiveFieldRead IO ()
afterSelectionDoneReactive w = reactivePropertyH_ w afterSelectionDone

-- @G: menuShellGetTakeFocus					 | ["menu","Shell","Get","Take","Focus"]
menuShellGetTakeFocusPassive :: MenuShellClass self => (self) -> ReactiveFieldRead IO (Bool)
menuShellGetTakeFocusPassive w = wrapMRPassive (menuShellGetTakeFocus w)


-- @T: menuShellSetTakeFocus					 | ["menu","Shell","Set","Take","Focus"]
menuShellSetTakeFocusReactive :: MenuShellClass self => (self) -> ReactiveFieldWrite IO (Bool)
menuShellSetTakeFocusReactive w = wrapMW (menuShellSetTakeFocus w)


-- @A: menuShellTakeFocus
menuShellTakeFocusPassive :: MenuShellClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
menuShellTakeFocusPassive w = passivePropertyNE w menuShellTakeFocus


-- @C: onActivateCurrent
-- TODO
-- @C: onCancel
onCancelReactive :: MenuShellClass self => self -> ReactiveFieldRead IO ()
onCancelReactive w = reactivePropertyH_ w onCancel

-- @C: onDeactivated
onDeactivatedReactive :: MenuShellClass self => self -> ReactiveFieldRead IO ()
onDeactivatedReactive w = reactivePropertyH_ w onDeactivated

-- @C: onMoveCurrent
-- TODO
-- @C: onSelectionDone
onSelectionDoneReactive :: MenuShellClass self => self -> ReactiveFieldRead IO ()
onSelectionDoneReactive w = reactivePropertyH_ w onSelectionDone

-- @C: afterShowMenu
afterShowMenuReactive :: MenuToolButtonClass self => self -> ReactiveFieldRead IO ()
afterShowMenuReactive w = reactivePropertyH_ w afterShowMenu

-- @G: menuToolButtonGetMenu					 | ["menu","Tool","Button","Get","Menu"]
menuToolButtonGetMenuPassive :: MenuToolButtonClass self => (self) -> ReactiveFieldRead IO ((Maybe Menu))
menuToolButtonGetMenuPassive w = wrapMRPassive (menuToolButtonGetMenu w)


-- @T: menuToolButtonSetArrowTooltip					 | ["menu","Tool","Button","Set","Arrow","Tooltip"]
-- TODO
-- @T: menuToolButtonSetArrowTooltipMarkup					 | ["menu","Tool","Button","Set","Arrow","Tooltip","Markup"]
menuToolButtonSetArrowTooltipMarkupReactive :: (MenuToolButtonClass self, GlibString markup) => (self) -> ReactiveFieldWrite IO (markup)
menuToolButtonSetArrowTooltipMarkupReactive w = wrapMW (menuToolButtonSetArrowTooltipMarkup w)


-- @T: menuToolButtonSetArrowTooltipText					 | ["menu","Tool","Button","Set","Arrow","Tooltip","Text"]
menuToolButtonSetArrowTooltipTextReactive :: (MenuToolButtonClass self, GlibString string) => (self) -> ReactiveFieldWrite IO (string)
menuToolButtonSetArrowTooltipTextReactive w = wrapMW (menuToolButtonSetArrowTooltipText w)


-- @T: menuToolButtonSetMenu					 | ["menu","Tool","Button","Set","Menu"]
menuToolButtonSetMenuReactive :: (MenuToolButtonClass self, MenuClass menu) => (self) -> ReactiveFieldWrite IO (Maybe menu)
menuToolButtonSetMenuReactive w = wrapMW (menuToolButtonSetMenu w)


-- @C: onShowMenu
onShowMenuReactive :: MenuToolButtonClass self => self -> ReactiveFieldRead IO ()
onShowMenuReactive w = reactivePropertyH_ w onShowMenu

-- @C: afterOMChanged
afterOMChangedReactive :: OptionMenuClass self => self -> ReactiveFieldRead IO ()
afterOMChangedReactive w = reactivePropertyH_ w afterOMChanged

-- @C: onOMChanged
onOMChangedReactive :: OptionMenuClass self => self -> ReactiveFieldRead IO ()
onOMChangedReactive w = reactivePropertyH_ w onOMChanged

-- @G: optionMenuGetHistory					 | ["option","Menu","Get","History"]
optionMenuGetHistoryPassive :: OptionMenuClass self => (self) -> ReactiveFieldRead IO (Int)
optionMenuGetHistoryPassive w = wrapMRPassive (optionMenuGetHistory w)


-- @G: optionMenuGetMenu					 | ["option","Menu","Get","Menu"]
optionMenuGetMenuPassive :: OptionMenuClass self => (self) -> ReactiveFieldRead IO (Menu)
optionMenuGetMenuPassive w = wrapMRPassive (optionMenuGetMenu w)


-- @T: optionMenuSetHistory					 | ["option","Menu","Set","History"]
optionMenuSetHistoryReactive :: OptionMenuClass self => (self) -> ReactiveFieldWrite IO (Int)
optionMenuSetHistoryReactive w = wrapMW (optionMenuSetHistory w)


-- @T: optionMenuSetMenu					 | ["option","Menu","Set","Menu"]
optionMenuSetMenuReactive :: (OptionMenuClass self, MenuClass menu) => (self) -> ReactiveFieldWrite IO (menu)
optionMenuSetMenuReactive w = wrapMW (optionMenuSetMenu w)


-- @G: radioToolButtonGetGroup					 | ["radio","Tool","Button","Get","Group"]
radioToolButtonGetGroupPassive :: RadioToolButtonClass self => (self) -> ReactiveFieldRead IO ([RadioToolButton])
radioToolButtonGetGroupPassive w = wrapMRPassive (radioToolButtonGetGroup w)


-- @T: radioToolButtonSetGroup					 | ["radio","Tool","Button","Set","Group"]
radioToolButtonSetGroupReactive :: RadioToolButtonClass self => (self) -> ReactiveFieldWrite IO (RadioToolButton)
radioToolButtonSetGroupReactive w = wrapMW (radioToolButtonSetGroup w)


-- @A: separatorToolItemDraw
separatorToolItemDrawPassive :: SeparatorToolItemClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
separatorToolItemDrawPassive w = passivePropertyNE w separatorToolItemDraw


-- @G: separatorToolItemGetDraw					 | ["separator","Tool","Item","Get","Draw"]
separatorToolItemGetDrawPassive :: SeparatorToolItemClass self => (self) -> ReactiveFieldRead IO (Bool)
separatorToolItemGetDrawPassive w = wrapMRPassive (separatorToolItemGetDraw w)


-- @T: separatorToolItemSetDraw					 | ["separator","Tool","Item","Set","Draw"]
separatorToolItemSetDrawReactive :: SeparatorToolItemClass self => (self) -> ReactiveFieldWrite IO (Bool)
separatorToolItemSetDrawReactive w = wrapMW (separatorToolItemSetDraw w)


-- @C: afterToolButtonToggled
afterToolButtonToggledReactive :: ToggleToolButtonClass self => self -> ReactiveFieldRead IO ()
afterToolButtonToggledReactive w = reactivePropertyH_ w afterToolButtonToggled

-- @C: onToolButtonToggled
onToolButtonToggledReactive :: ToggleToolButtonClass self => self -> ReactiveFieldRead IO ()
onToolButtonToggledReactive w = reactivePropertyH_ w onToolButtonToggled

-- @A: toggleToolButtonActive
toggleToolButtonActivePassive :: ToggleToolButtonClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
toggleToolButtonActivePassive w = passivePropertyNE w toggleToolButtonActive


-- @G: toggleToolButtonGetActive					 | ["toggle","Tool","Button","Get","Active"]
toggleToolButtonGetActivePassive :: ToggleToolButtonClass self => (self) -> ReactiveFieldRead IO (Bool)
toggleToolButtonGetActivePassive w = wrapMRPassive (toggleToolButtonGetActive w)


-- @T: toggleToolButtonSetActive					 | ["toggle","Tool","Button","Set","Active"]
toggleToolButtonSetActiveReactive :: ToggleToolButtonClass self => (self) -> ReactiveFieldWrite IO (Bool)
toggleToolButtonSetActiveReactive w = wrapMW (toggleToolButtonSetActive w)


-- @C: afterOrientationChanged
-- TODO
-- @C: afterPopupContextMenu
-- TODO
-- @C: afterStyleChanged
-- TODO
-- @C: onOrientationChanged
-- TODO
-- @C: onPopupContextMenu
-- TODO
-- @C: onStyleChanged
-- TODO
-- @A: toolbarChildExpand
-- TODO
-- @A: toolbarChildHomogeneous
-- TODO
-- @G: toolbarGetDropIndex					 | ["toolbar","Get","Drop","Index"]
-- TODO
-- @G: toolbarGetIconSize					 | ["toolbar","Get","Icon","Size"]
toolbarGetIconSizePassive :: ToolbarClass self => (self) -> ReactiveFieldRead IO (IconSize)
toolbarGetIconSizePassive w = wrapMRPassive (toolbarGetIconSize w)


-- @G: toolbarGetItemIndex					 | ["toolbar","Get","Item","Index"]
-- TODO
-- @G: toolbarGetNItems					 | ["toolbar","Get","NItems"]
toolbarGetNItemsPassive :: ToolbarClass self => (self) -> ReactiveFieldRead IO (Int)
toolbarGetNItemsPassive w = wrapMRPassive (toolbarGetNItems w)


-- @G: toolbarGetNthItem					 | ["toolbar","Get","Nth","Item"]
-- TODO
-- @G: toolbarGetOrientation					 | ["toolbar","Get","Orientation"]
toolbarGetOrientationPassive :: ToolbarClass self => (self) -> ReactiveFieldRead IO (Orientation)
toolbarGetOrientationPassive w = wrapMRPassive (toolbarGetOrientation w)


-- @G: toolbarGetReliefStyle					 | ["toolbar","Get","Relief","Style"]
toolbarGetReliefStylePassive :: ToolbarClass self => (self) -> ReactiveFieldRead IO (ReliefStyle)
toolbarGetReliefStylePassive w = wrapMRPassive (toolbarGetReliefStyle w)


-- @G: toolbarGetShowArrow					 | ["toolbar","Get","Show","Arrow"]
toolbarGetShowArrowPassive :: ToolbarClass self => (self) -> ReactiveFieldRead IO (Bool)
toolbarGetShowArrowPassive w = wrapMRPassive (toolbarGetShowArrow w)


-- @G: toolbarGetStyle					 | ["toolbar","Get","Style"]
toolbarGetStylePassive :: ToolbarClass self => (self) -> ReactiveFieldRead IO (ToolbarStyle)
toolbarGetStylePassive w = wrapMRPassive (toolbarGetStyle w)


-- @G: toolbarGetTooltips					 | ["toolbar","Get","Tooltips"]
toolbarGetTooltipsPassive :: ToolbarClass self => (self) -> ReactiveFieldRead IO (Bool)
toolbarGetTooltipsPassive w = wrapMRPassive (toolbarGetTooltips w)


-- @A: toolbarOrientation
toolbarOrientationPassive :: ToolbarClass self => (self) -> ReactiveFieldReadWrite IO (Orientation)
toolbarOrientationPassive w = passivePropertyNE w toolbarOrientation


-- @T: toolbarSetDropHighlightItem					 | ["toolbar","Set","Drop","Highlight","Item"]
-- TODO
-- @T: toolbarSetIconSize					 | ["toolbar","Set","Icon","Size"]
toolbarSetIconSizeReactive :: ToolbarClass self => (self) -> ReactiveFieldWrite IO (IconSize)
toolbarSetIconSizeReactive w = wrapMW (toolbarSetIconSize w)


-- @T: toolbarSetOrientation					 | ["toolbar","Set","Orientation"]
toolbarSetOrientationReactive :: ToolbarClass self => (self) -> ReactiveFieldWrite IO (Orientation)
toolbarSetOrientationReactive w = wrapMW (toolbarSetOrientation w)


-- @T: toolbarSetShowArrow					 | ["toolbar","Set","Show","Arrow"]
toolbarSetShowArrowReactive :: ToolbarClass self => (self) -> ReactiveFieldWrite IO (Bool)
toolbarSetShowArrowReactive w = wrapMW (toolbarSetShowArrow w)


-- @T: toolbarSetStyle					 | ["toolbar","Set","Style"]
toolbarSetStyleReactive :: ToolbarClass self => (self) -> ReactiveFieldWrite IO (ToolbarStyle)
toolbarSetStyleReactive w = wrapMW (toolbarSetStyle w)


-- @T: toolbarSetTooltips					 | ["toolbar","Set","Tooltips"]
toolbarSetTooltipsReactive :: ToolbarClass self => (self) -> ReactiveFieldWrite IO (Bool)
toolbarSetTooltipsReactive w = wrapMW (toolbarSetTooltips w)


-- @A: toolbarShowArrow
toolbarShowArrowPassive :: ToolbarClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
toolbarShowArrowPassive w = passivePropertyNE w toolbarShowArrow


-- @A: toolbarStyle
toolbarStylePassive :: ToolbarClass self => (self) -> ReactiveFieldReadWrite IO (ToolbarStyle)
toolbarStylePassive w = passivePropertyNE w toolbarStyle


-- @A: toolbarTooltips
toolbarTooltipsPassive :: ToolbarClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
toolbarTooltipsPassive w = passivePropertyNE w toolbarTooltips


-- @C: afterToolButtonClicked
afterToolButtonClickedReactive :: ToolButtonClass self => self -> ReactiveFieldRead IO ()
afterToolButtonClickedReactive w = reactivePropertyH_ w afterToolButtonClicked

-- @C: onToolButtonClicked
onToolButtonClickedReactive :: ToolButtonClass self => self -> ReactiveFieldRead IO ()
onToolButtonClickedReactive w = reactivePropertyH_ w onToolButtonClicked

-- @G: toolButtonGetIconName					 | ["tool","Button","Get","Icon","Name"]
toolButtonGetIconNamePassive :: (ToolButtonClass self, GlibString string) => (self) -> ReactiveFieldRead IO (string)
toolButtonGetIconNamePassive w = wrapMRPassive (toolButtonGetIconName w)


-- @G: toolButtonGetIconWidget					 | ["tool","Button","Get","Icon","Widget"]
toolButtonGetIconWidgetPassive :: ToolButtonClass self => (self) -> ReactiveFieldRead IO ((Maybe Widget))
toolButtonGetIconWidgetPassive w = wrapMRPassive (toolButtonGetIconWidget w)


-- @G: toolButtonGetLabel					 | ["tool","Button","Get","Label"]
toolButtonGetLabelPassive :: (ToolButtonClass self, GlibString string) => (self) -> ReactiveFieldRead IO ((Maybe string))
toolButtonGetLabelPassive w = wrapMRPassive (toolButtonGetLabel w)


-- @G: toolButtonGetLabelWidget					 | ["tool","Button","Get","Label","Widget"]
toolButtonGetLabelWidgetPassive :: ToolButtonClass self => (self) -> ReactiveFieldRead IO ((Maybe Widget))
toolButtonGetLabelWidgetPassive w = wrapMRPassive (toolButtonGetLabelWidget w)


-- @G: toolButtonGetStockId					 | ["tool","Button","Get","Stock","Id"]
toolButtonGetStockIdPassive :: ToolButtonClass self => (self) -> ReactiveFieldRead IO ((Maybe StockId))
toolButtonGetStockIdPassive w = wrapMRPassive (toolButtonGetStockId w)


-- @G: toolButtonGetUseUnderline					 | ["tool","Button","Get","Use","Underline"]
toolButtonGetUseUnderlinePassive :: ToolButtonClass self => (self) -> ReactiveFieldRead IO (Bool)
toolButtonGetUseUnderlinePassive w = wrapMRPassive (toolButtonGetUseUnderline w)


-- @A: toolButtonIconName
toolButtonIconNamePassive :: (ToolButtonClass self, GlibString string) => (self) -> ReactiveFieldReadWrite IO (string)
toolButtonIconNamePassive w = passivePropertyNE w toolButtonIconName


-- @A: toolButtonLabel
toolButtonLabelPassive :: (ToolButtonClass self, GlibString string) => (self) -> ReactiveFieldReadWrite IO ((Maybe string))
toolButtonLabelPassive w = passivePropertyNE w toolButtonLabel


-- @T: toolButtonSetIconName					 | ["tool","Button","Set","Icon","Name"]
toolButtonSetIconNameReactive :: (ToolButtonClass self, GlibString string) => (self) -> ReactiveFieldWrite IO (string)
toolButtonSetIconNameReactive w = wrapMW (toolButtonSetIconName w)


-- @T: toolButtonSetIconWidget					 | ["tool","Button","Set","Icon","Widget"]
toolButtonSetIconWidgetReactive :: (ToolButtonClass self, WidgetClass iconWidget) => (self) -> ReactiveFieldWrite IO (Maybe iconWidget)
toolButtonSetIconWidgetReactive w = wrapMW (toolButtonSetIconWidget w)


-- @T: toolButtonSetLabel					 | ["tool","Button","Set","Label"]
toolButtonSetLabelReactive :: (ToolButtonClass self, GlibString string) => (self) -> ReactiveFieldWrite IO (Maybe string)
toolButtonSetLabelReactive w = wrapMW (toolButtonSetLabel w)


-- @T: toolButtonSetLabelWidget					 | ["tool","Button","Set","Label","Widget"]
toolButtonSetLabelWidgetReactive :: (ToolButtonClass self, WidgetClass labelWidget) => (self) -> ReactiveFieldWrite IO (Maybe labelWidget)
toolButtonSetLabelWidgetReactive w = wrapMW (toolButtonSetLabelWidget w)


-- @T: toolButtonSetStockId					 | ["tool","Button","Set","Stock","Id"]
toolButtonSetStockIdReactive :: ToolButtonClass self => (self) -> ReactiveFieldWrite IO (Maybe StockId)
toolButtonSetStockIdReactive w = wrapMW (toolButtonSetStockId w)


-- @T: toolButtonSetUseUnderline					 | ["tool","Button","Set","Use","Underline"]
toolButtonSetUseUnderlineReactive :: ToolButtonClass self => (self) -> ReactiveFieldWrite IO (Bool)
toolButtonSetUseUnderlineReactive w = wrapMW (toolButtonSetUseUnderline w)


-- @A: toolButtonUseUnderline
toolButtonUseUnderlinePassive :: ToolButtonClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
toolButtonUseUnderlinePassive w = passivePropertyNE w toolButtonUseUnderline


-- @A: toolItemGroupChildExpand
toolItemGroupChildExpandPassive :: ToolItemGroupClass group => (group) -> ReactiveFieldReadWrite IO (Bool)
toolItemGroupChildExpandPassive w = passivePropertyNE w toolItemGroupChildExpand


-- @A: toolItemGroupChildFill
toolItemGroupChildFillPassive :: ToolItemGroupClass group => (group) -> ReactiveFieldReadWrite IO (Bool)
toolItemGroupChildFillPassive w = passivePropertyNE w toolItemGroupChildFill


-- @A: toolItemGroupChildHomogeneous
toolItemGroupChildHomogeneousPassive :: ToolItemGroupClass group => (group) -> ReactiveFieldReadWrite IO (Bool)
toolItemGroupChildHomogeneousPassive w = passivePropertyNE w toolItemGroupChildHomogeneous


-- @A: toolItemGroupChildNewRow
toolItemGroupChildNewRowPassive :: ToolItemGroupClass group => (group) -> ReactiveFieldReadWrite IO (Bool)
toolItemGroupChildNewRowPassive w = passivePropertyNE w toolItemGroupChildNewRow


-- @A: toolItemGroupChildPosition
toolItemGroupChildPositionPassive :: ToolItemGroupClass group => (group) -> ReactiveFieldReadWrite IO (Int)
toolItemGroupChildPositionPassive w = passivePropertyNE w toolItemGroupChildPosition


-- @A: toolItemGroupCollapsed
toolItemGroupCollapsedPassive :: ToolItemGroupClass group => (group) -> ReactiveFieldReadWrite IO (Bool)
toolItemGroupCollapsedPassive w = passivePropertyNE w toolItemGroupCollapsed


-- @A: toolItemGroupEllipsize
toolItemGroupEllipsizePassive :: ToolItemGroupClass group => (group) -> ReactiveFieldReadWrite IO (EllipsizeMode)
toolItemGroupEllipsizePassive w = passivePropertyNE w toolItemGroupEllipsize


-- @G: toolItemGroupGetDropItem					 | ["tool","Item","Group","Get","Drop","Item"]
-- TODO
-- @G: toolItemGroupGetItemPosition					 | ["tool","Item","Group","Get","Item","Position"]
-- TODO
-- @G: toolItemGroupGetNItems					 | ["tool","Item","Group","Get","NItems"]
toolItemGroupGetNItemsPassive :: ToolItemGroupClass group => (group) -> ReactiveFieldRead IO (Int)
toolItemGroupGetNItemsPassive w = wrapMRPassive (toolItemGroupGetNItems w)


-- @G: toolItemGroupGetNthItem					 | ["tool","Item","Group","Get","Nth","Item"]
-- TODO
-- @A: toolItemGroupHeaderRelief
toolItemGroupHeaderReliefPassive :: ToolItemGroupClass group => (group) -> ReactiveFieldReadWrite IO (ReliefStyle)
toolItemGroupHeaderReliefPassive w = passivePropertyNE w toolItemGroupHeaderRelief


-- @A: toolItemGroupLabel
toolItemGroupLabelPassive :: (GlibString string, ToolItemGroupClass group) => (group) -> ReactiveFieldReadWrite IO (string)
toolItemGroupLabelPassive w = passivePropertyNE w toolItemGroupLabel


-- @A: toolItemGroupLabelWidget
toolItemGroupLabelWidgetPassive :: ToolItemGroupClass group => (group) -> ReactiveFieldReadWrite IO (Widget)
toolItemGroupLabelWidgetPassive w = passivePropertyNE w toolItemGroupLabelWidget


-- @T: toolItemGroupSetItemPosition					 | ["tool","Item","Group","Set","Item","Position"]
-- TODO
-- @A: toolItemExpand
toolItemExpandPassive :: ToolItemClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
toolItemExpandPassive w = passivePropertyNE w toolItemExpand


-- @G: toolItemGetEllipsizeMode					 | ["tool","Item","Get","Ellipsize","Mode"]
toolItemGetEllipsizeModePassive :: ToolItemClass item => (item) -> ReactiveFieldRead IO (EllipsizeMode)
toolItemGetEllipsizeModePassive w = wrapMRPassive (toolItemGetEllipsizeMode w)


-- @G: toolItemGetExpand					 | ["tool","Item","Get","Expand"]
toolItemGetExpandPassive :: ToolItemClass self => (self) -> ReactiveFieldRead IO (Bool)
toolItemGetExpandPassive w = wrapMRPassive (toolItemGetExpand w)


-- @G: toolItemGetHomogeneous					 | ["tool","Item","Get","Homogeneous"]
toolItemGetHomogeneousPassive :: ToolItemClass self => (self) -> ReactiveFieldRead IO (Bool)
toolItemGetHomogeneousPassive w = wrapMRPassive (toolItemGetHomogeneous w)


-- @G: toolItemGetIconSize					 | ["tool","Item","Get","Icon","Size"]
toolItemGetIconSizePassive :: ToolItemClass self => (self) -> ReactiveFieldRead IO (IconSize)
toolItemGetIconSizePassive w = wrapMRPassive (toolItemGetIconSize w)


-- @G: toolItemGetIsImportant					 | ["tool","Item","Get","Is","Important"]
toolItemGetIsImportantPassive :: ToolItemClass self => (self) -> ReactiveFieldRead IO (Bool)
toolItemGetIsImportantPassive w = wrapMRPassive (toolItemGetIsImportant w)


-- @G: toolItemGetOrientation					 | ["tool","Item","Get","Orientation"]
toolItemGetOrientationPassive :: ToolItemClass self => (self) -> ReactiveFieldRead IO (Orientation)
toolItemGetOrientationPassive w = wrapMRPassive (toolItemGetOrientation w)


-- @G: toolItemGetProxyMenuItem					 | ["tool","Item","Get","Proxy","Menu","Item"]
-- TODO
-- @G: toolItemGetReliefStyle					 | ["tool","Item","Get","Relief","Style"]
toolItemGetReliefStylePassive :: ToolItemClass self => (self) -> ReactiveFieldRead IO (ReliefStyle)
toolItemGetReliefStylePassive w = wrapMRPassive (toolItemGetReliefStyle w)


-- @G: toolItemGetTextAlignment					 | ["tool","Item","Get","Text","Alignment"]
toolItemGetTextAlignmentPassive :: ToolItemClass item => (item) -> ReactiveFieldRead IO (Double)
toolItemGetTextAlignmentPassive w = wrapMRPassive (toolItemGetTextAlignment w)


-- @G: toolItemGetTextOrientation					 | ["tool","Item","Get","Text","Orientation"]
toolItemGetTextOrientationPassive :: ToolItemClass item => (item) -> ReactiveFieldRead IO (Orientation)
toolItemGetTextOrientationPassive w = wrapMRPassive (toolItemGetTextOrientation w)


-- @G: toolItemGetTextSizeGroup					 | ["tool","Item","Get","Text","Size","Group"]
toolItemGetTextSizeGroupPassive :: ToolItemClass item => (item) -> ReactiveFieldRead IO (SizeGroup)
toolItemGetTextSizeGroupPassive w = wrapMRPassive (toolItemGetTextSizeGroup w)


-- @G: toolItemGetToolbarStyle					 | ["tool","Item","Get","Toolbar","Style"]
toolItemGetToolbarStylePassive :: ToolItemClass self => (self) -> ReactiveFieldRead IO (ToolbarStyle)
toolItemGetToolbarStylePassive w = wrapMRPassive (toolItemGetToolbarStyle w)


-- @G: toolItemGetUseDragWindow					 | ["tool","Item","Get","Use","Drag","Window"]
toolItemGetUseDragWindowPassive :: ToolItemClass self => (self) -> ReactiveFieldRead IO (Bool)
toolItemGetUseDragWindowPassive w = wrapMRPassive (toolItemGetUseDragWindow w)


-- @G: toolItemGetVisibleHorizontal					 | ["tool","Item","Get","Visible","Horizontal"]
toolItemGetVisibleHorizontalPassive :: ToolItemClass self => (self) -> ReactiveFieldRead IO (Bool)
toolItemGetVisibleHorizontalPassive w = wrapMRPassive (toolItemGetVisibleHorizontal w)


-- @G: toolItemGetVisibleVertical					 | ["tool","Item","Get","Visible","Vertical"]
toolItemGetVisibleVerticalPassive :: ToolItemClass self => (self) -> ReactiveFieldRead IO (Bool)
toolItemGetVisibleVerticalPassive w = wrapMRPassive (toolItemGetVisibleVertical w)


-- @A: toolItemHomogeneous
toolItemHomogeneousPassive :: ToolItemClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
toolItemHomogeneousPassive w = passivePropertyNE w toolItemHomogeneous


-- @A: toolItemIsImportant
toolItemIsImportantPassive :: ToolItemClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
toolItemIsImportantPassive w = passivePropertyNE w toolItemIsImportant


-- @T: toolItemSetExpand					 | ["tool","Item","Set","Expand"]
toolItemSetExpandReactive :: ToolItemClass self => (self) -> ReactiveFieldWrite IO (Bool)
toolItemSetExpandReactive w = wrapMW (toolItemSetExpand w)


-- @T: toolItemSetHomogeneous					 | ["tool","Item","Set","Homogeneous"]
toolItemSetHomogeneousReactive :: ToolItemClass self => (self) -> ReactiveFieldWrite IO (Bool)
toolItemSetHomogeneousReactive w = wrapMW (toolItemSetHomogeneous w)


-- @T: toolItemSetIsImportant					 | ["tool","Item","Set","Is","Important"]
toolItemSetIsImportantReactive :: ToolItemClass self => (self) -> ReactiveFieldWrite IO (Bool)
toolItemSetIsImportantReactive w = wrapMW (toolItemSetIsImportant w)


-- @T: toolItemSetProxyMenuItem					 | ["tool","Item","Set","Proxy","Menu","Item"]
-- TODO
-- @T: toolItemSetTooltip					 | ["tool","Item","Set","Tooltip"]
-- TODO
-- @T: toolItemSetUseDragWindow					 | ["tool","Item","Set","Use","Drag","Window"]
toolItemSetUseDragWindowReactive :: ToolItemClass self => (self) -> ReactiveFieldWrite IO (Bool)
toolItemSetUseDragWindowReactive w = wrapMW (toolItemSetUseDragWindow w)


-- @T: toolItemSetVisibleHorizontal					 | ["tool","Item","Set","Visible","Horizontal"]
toolItemSetVisibleHorizontalReactive :: ToolItemClass self => (self) -> ReactiveFieldWrite IO (Bool)
toolItemSetVisibleHorizontalReactive w = wrapMW (toolItemSetVisibleHorizontal w)


-- @T: toolItemSetVisibleVertical					 | ["tool","Item","Set","Visible","Vertical"]
toolItemSetVisibleVerticalReactive :: ToolItemClass self => (self) -> ReactiveFieldWrite IO (Bool)
toolItemSetVisibleVerticalReactive w = wrapMW (toolItemSetVisibleVertical w)


-- @A: toolItemUseDragWindow
toolItemUseDragWindowPassive :: ToolItemClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
toolItemUseDragWindowPassive w = passivePropertyNE w toolItemUseDragWindow


-- @A: toolItemVisibleHorizontal
toolItemVisibleHorizontalPassive :: ToolItemClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
toolItemVisibleHorizontalPassive w = passivePropertyNE w toolItemVisibleHorizontal


-- @A: toolItemVisibleVertical
toolItemVisibleVerticalPassive :: ToolItemClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
toolItemVisibleVerticalPassive w = passivePropertyNE w toolItemVisibleVertical


-- @A: toolPaletteChildExclusive
toolPaletteChildExclusivePassive :: ToolPaletteClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
toolPaletteChildExclusivePassive w = passivePropertyNE w toolPaletteChildExclusive


-- @A: toolPaletteChildExpand
toolPaletteChildExpandPassive :: ToolPaletteClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
toolPaletteChildExpandPassive w = passivePropertyNE w toolPaletteChildExpand


-- @G: toolPaletteGetGroupPosition					 | ["tool","Palette","Get","Group","Position"]
-- TODO
-- @G: toolPaletteGetHAdjustment					 | ["tool","Palette","Get","HAdjustment"]
toolPaletteGetHAdjustmentPassive :: ToolPaletteClass self => (self) -> ReactiveFieldRead IO (Adjustment)
toolPaletteGetHAdjustmentPassive w = wrapMRPassive (toolPaletteGetHAdjustment w)


-- @G: toolPaletteGetVAdjustment					 | ["tool","Palette","Get","VAdjustment"]
toolPaletteGetVAdjustmentPassive :: ToolPaletteClass self => (self) -> ReactiveFieldRead IO (Adjustment)
toolPaletteGetVAdjustmentPassive w = wrapMRPassive (toolPaletteGetVAdjustment w)


-- @A: toolPaletteIconSize
toolPaletteIconSizePassive :: ToolPaletteClass self => (self) -> ReactiveFieldReadWrite IO (IconSize)
toolPaletteIconSizePassive w = passivePropertyNE w toolPaletteIconSize


-- @A: toolPaletteIconSizeSet
toolPaletteIconSizeSetPassive :: ToolPaletteClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
toolPaletteIconSizeSetPassive w = passivePropertyNE w toolPaletteIconSizeSet


-- @T: toolPaletteSetGroupPosition					 | ["tool","Palette","Set","Group","Position"]
-- TODO
-- @S: toolPaletteSetScrollAdjustments
-- TODO
-- @A: toolPaletteToolbarStyle
toolPaletteToolbarStylePassive :: ToolPaletteClass self => (self) -> ReactiveFieldReadWrite IO (ToolbarStyle)
toolPaletteToolbarStylePassive w = passivePropertyNE w toolPaletteToolbarStyle


-- @G: accessibleGetWidget					 | ["accessible","Get","Widget"]
accessibleGetWidgetPassive :: AccessibleClass self => (self) -> ReactiveFieldRead IO ((Maybe Widget))
accessibleGetWidgetPassive w = wrapMRPassive (accessibleGetWidget w)


-- @T: accessibleSetWidget					 | ["accessible","Set","Widget"]
accessibleSetWidgetReactive :: (AccessibleClass self, WidgetClass widget) => (self) -> ReactiveFieldWrite IO (widget)
accessibleSetWidgetReactive w = wrapMW (accessibleSetWidget w)


-- @G: adjustmentGetLower					 | ["adjustment","Get","Lower"]
adjustmentGetLowerPassive :: (Adjustment) -> ReactiveFieldRead IO (Double)
adjustmentGetLowerPassive w = wrapMRPassive (adjustmentGetLower w)


-- @G: adjustmentGetPageIncrement					 | ["adjustment","Get","Page","Increment"]
adjustmentGetPageIncrementPassive :: (Adjustment) -> ReactiveFieldRead IO (Double)
adjustmentGetPageIncrementPassive w = wrapMRPassive (adjustmentGetPageIncrement w)


-- @G: adjustmentGetPageSize					 | ["adjustment","Get","Page","Size"]
adjustmentGetPageSizePassive :: (Adjustment) -> ReactiveFieldRead IO (Double)
adjustmentGetPageSizePassive w = wrapMRPassive (adjustmentGetPageSize w)


-- @G: adjustmentGetStepIncrement					 | ["adjustment","Get","Step","Increment"]
adjustmentGetStepIncrementPassive :: (Adjustment) -> ReactiveFieldRead IO (Double)
adjustmentGetStepIncrementPassive w = wrapMRPassive (adjustmentGetStepIncrement w)


-- @G: adjustmentGetUpper					 | ["adjustment","Get","Upper"]
adjustmentGetUpperPassive :: (Adjustment) -> ReactiveFieldRead IO (Double)
adjustmentGetUpperPassive w = wrapMRPassive (adjustmentGetUpper w)


-- @G: adjustmentGetValue					 | ["adjustment","Get","Value"]
adjustmentGetValuePassive :: (Adjustment) -> ReactiveFieldRead IO (Double)
adjustmentGetValuePassive w = wrapMRPassive (adjustmentGetValue w)


-- @A: adjustmentLower
adjustmentLowerPassive :: Adjustment -> ReactiveFieldReadWrite IO Double
adjustmentLowerPassive w = passivePropertyNE w adjustmentLower


-- @A: adjustmentPageIncrement
adjustmentPageIncrementPassive :: Adjustment -> ReactiveFieldReadWrite IO Double
adjustmentPageIncrementPassive w = passivePropertyNE w adjustmentPageIncrement


-- @A: adjustmentPageSize
adjustmentPageSizePassive :: Adjustment -> ReactiveFieldReadWrite IO Double
adjustmentPageSizePassive w = passivePropertyNE w adjustmentPageSize


-- @T: adjustmentSetLower					 | ["adjustment","Set","Lower"]
adjustmentSetLowerPassive :: (Adjustment) -> ReactiveFieldWrite IO (Double)
adjustmentSetLowerPassive w = wrapMW (adjustmentSetLower w)

-- @T: adjustmentSetPageIncrement					 | ["adjustment","Set","Page","Increment"]
adjustmentSetPageIncrementPassive :: (Adjustment) -> ReactiveFieldWrite IO (Double)
adjustmentSetPageIncrementPassive w = wrapMW (adjustmentSetPageIncrement w)

-- @T: adjustmentSetPageSize					 | ["adjustment","Set","Page","Size"]
adjustmentSetPageSizePassive :: (Adjustment) -> ReactiveFieldWrite IO (Double)
adjustmentSetPageSizePassive w = wrapMW (adjustmentSetPageSize w)

-- @T: adjustmentSetStepIncrement					 | ["adjustment","Set","Step","Increment"]
adjustmentSetStepIncrementPassive :: (Adjustment) -> ReactiveFieldWrite IO (Double)
adjustmentSetStepIncrementPassive w = wrapMW (adjustmentSetStepIncrement w)

-- @T: adjustmentSetUpper					 | ["adjustment","Set","Upper"]
adjustmentSetUpperPassive :: (Adjustment) -> ReactiveFieldWrite IO (Double)
adjustmentSetUpperPassive w = wrapMW (adjustmentSetUpper w)

-- @T: adjustmentSetValue					 | ["adjustment","Set","Value"]
adjustmentSetValuePassive :: (Adjustment) -> ReactiveFieldWrite IO (Double)
adjustmentSetValuePassive w = wrapMW (adjustmentSetValue w)

-- @A: adjustmentStepIncrement
adjustmentStepIncrementPassive :: Adjustment -> ReactiveFieldReadWrite IO Double
adjustmentStepIncrementPassive w = passivePropertyNE w adjustmentStepIncrement


-- @A: adjustmentUpper
adjustmentUpperPassive :: Adjustment -> ReactiveFieldReadWrite IO Double
adjustmentUpperPassive w = passivePropertyNE w adjustmentUpper


-- @A: adjustmentValue
adjustmentValuePassive :: Adjustment -> ReactiveFieldReadWrite IO Double
adjustmentValuePassive w = passivePropertyNE w adjustmentValue


-- @C: afterAdjChanged
afterAdjChangedReactive :: Adjustment -> ReactiveFieldRead IO ()
afterAdjChangedReactive w = reactivePropertyH_ w afterAdjChanged

-- @C: afterValueChanged
afterValueChangedReactive :: Adjustment -> ReactiveFieldRead IO ()
afterValueChangedReactive w = reactivePropertyH_ w afterValueChanged

-- @C: onAdjChanged
onAdjChangedReactive :: Adjustment -> ReactiveFieldRead IO ()
onAdjChangedReactive w = reactivePropertyH_ w onAdjChanged

-- @C: onValueChanged
onValueChangedReactive :: Adjustment -> ReactiveFieldRead IO ()
onValueChangedReactive w = reactivePropertyH_ w onValueChanged

-- @A: arrowArrowType
arrowArrowTypePassive :: ArrowClass self => (self) -> ReactiveFieldReadWrite IO (ArrowType)
arrowArrowTypePassive w = passivePropertyNE w arrowArrowType


-- @T: arrowSet					 | ["arrow","Set"]
-- TODO
-- @A: arrowShadowType
arrowShadowTypePassive :: ArrowClass self => (self) -> ReactiveFieldReadWrite IO (ShadowType)
arrowShadowTypePassive w = passivePropertyNE w arrowShadowType


-- @C: afterDaySelectedDoubleClick
afterDaySelectedDoubleClickReactive :: CalendarClass self => self -> ReactiveFieldRead IO ()
afterDaySelectedDoubleClickReactive w = reactivePropertyH_ w afterDaySelectedDoubleClick

-- @C: afterDaySelected
afterDaySelectedReactive :: CalendarClass self => self -> ReactiveFieldRead IO ()
afterDaySelectedReactive w = reactivePropertyH_ w afterDaySelected

-- @C: afterMonthChanged
afterMonthChangedReactive :: CalendarClass self => self -> ReactiveFieldRead IO ()
afterMonthChangedReactive w = reactivePropertyH_ w afterMonthChanged

-- @C: afterNextMonth
afterNextMonthReactive :: CalendarClass self => self -> ReactiveFieldRead IO ()
afterNextMonthReactive w = reactivePropertyH_ w afterNextMonth

-- @C: afterNextYear
afterNextYearReactive :: CalendarClass self => self -> ReactiveFieldRead IO ()
afterNextYearReactive w = reactivePropertyH_ w afterNextYear

-- @C: afterPrevMonth
afterPrevMonthReactive :: CalendarClass self => self -> ReactiveFieldRead IO ()
afterPrevMonthReactive w = reactivePropertyH_ w afterPrevMonth

-- @C: afterPrevYear
afterPrevYearReactive :: CalendarClass self => self -> ReactiveFieldRead IO ()
afterPrevYearReactive w = reactivePropertyH_ w afterPrevYear

-- @A: calendarDay
calendarDayPassive :: CalendarClass self => (self) -> ReactiveFieldReadWrite IO (Int)
calendarDayPassive w = passivePropertyNE w calendarDay


-- @G: calendarGetDate					 | ["calendar","Get","Date"]
calendarGetDatePassive :: CalendarClass self => (self) -> ReactiveFieldRead IO ((Int, Int, Int))
calendarGetDatePassive w = wrapMRPassive (calendarGetDate w)


-- @G: calendarGetDisplayOptions					 | ["calendar","Get","Display","Options"]
calendarGetDisplayOptionsPassive :: CalendarClass self => (self) -> ReactiveFieldRead IO ([CalendarDisplayOptions])
calendarGetDisplayOptionsPassive w = wrapMRPassive (calendarGetDisplayOptions w)


-- @A: calendarMonth
calendarMonthPassive :: CalendarClass self => (self) -> ReactiveFieldReadWrite IO (Int)
calendarMonthPassive w = passivePropertyNE w calendarMonth


-- @A: calendarNoMonthChange
calendarNoMonthChangePassive :: CalendarClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
calendarNoMonthChangePassive w = passivePropertyNE w calendarNoMonthChange


-- @T: calendarSetDisplayOptions					 | ["calendar","Set","Display","Options"]
calendarSetDisplayOptionsReactive :: CalendarClass self => (self) -> ReactiveFieldWrite IO ([CalendarDisplayOptions])
calendarSetDisplayOptionsReactive w = wrapMW (calendarSetDisplayOptions w)


-- @A: calendarShowDayNames
calendarShowDayNamesPassive :: CalendarClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
calendarShowDayNamesPassive w = passivePropertyNE w calendarShowDayNames


-- @A: calendarShowHeading
calendarShowHeadingPassive :: CalendarClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
calendarShowHeadingPassive w = passivePropertyNE w calendarShowHeading


-- @A: calendarShowWeekNumbers
calendarShowWeekNumbersPassive :: CalendarClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
calendarShowWeekNumbersPassive w = passivePropertyNE w calendarShowWeekNumbers


-- @A: calendarYear
calendarYearPassive :: CalendarClass self => (self) -> ReactiveFieldReadWrite IO (Int)
calendarYearPassive w = passivePropertyNE w calendarYear


-- @C: onDaySelectedDoubleClick
onDaySelectedDoubleClickReactive :: CalendarClass self => self -> ReactiveFieldRead IO ()
onDaySelectedDoubleClickReactive w = reactivePropertyH_ w onDaySelectedDoubleClick

-- @C: onDaySelected
onDaySelectedReactive :: CalendarClass self => self -> ReactiveFieldRead IO ()
onDaySelectedReactive w = reactivePropertyH_ w onDaySelected

-- @C: onMonthChanged
onMonthChangedReactive :: CalendarClass self => self -> ReactiveFieldRead IO ()
onMonthChangedReactive w = reactivePropertyH_ w onMonthChanged

-- @C: onNextMonth
onNextMonthReactive :: CalendarClass self => self -> ReactiveFieldRead IO ()
onNextMonthReactive w = reactivePropertyH_ w onNextMonth

-- @C: onNextYear
onNextYearReactive :: CalendarClass self => self -> ReactiveFieldRead IO ()
onNextYearReactive w = reactivePropertyH_ w onNextYear

-- @C: onPrevMonth
onPrevMonthReactive :: CalendarClass self => self -> ReactiveFieldRead IO ()
onPrevMonthReactive w = reactivePropertyH_ w onPrevMonth

-- @C: onPrevYear
onPrevYearReactive :: CalendarClass self => self -> ReactiveFieldRead IO ()
onPrevYearReactive w = reactivePropertyH_ w onPrevYear

-- @G: drawingAreaGetDrawWindow					 | ["drawing","Area","Get","Draw","Window"]
drawingAreaGetDrawWindowPassive :: (DrawingArea) -> ReactiveFieldRead IO (DrawWindow)
drawingAreaGetDrawWindowPassive w = wrapMRPassive (drawingAreaGetDrawWindow w)


-- @G: drawingAreaGetSize					 | ["drawing","Area","Get","Size"]
drawingAreaGetSizePassive :: (DrawingArea) -> ReactiveFieldRead IO ((Int, Int))
drawingAreaGetSizePassive w = wrapMRPassive (drawingAreaGetSize w)


-- @A: eventBoxAboveChild
eventBoxAboveChildPassive :: EventBox -> ReactiveFieldReadWrite IO Bool
eventBoxAboveChildPassive w = passivePropertyNE w eventBoxAboveChild


-- @G: eventBoxGetAboveChild					 | ["event","Box","Get","Above","Child"]
eventBoxGetAboveChildPassive :: (EventBox) -> ReactiveFieldRead IO (Bool)
eventBoxGetAboveChildPassive w = wrapMRPassive (eventBoxGetAboveChild w)


-- @G: eventBoxGetVisibleWindow					 | ["event","Box","Get","Visible","Window"]
eventBoxGetVisibleWindowPassive :: (EventBox) -> ReactiveFieldRead IO (Bool)
eventBoxGetVisibleWindowPassive w = wrapMRPassive (eventBoxGetVisibleWindow w)


-- @T: eventBoxSetAboveChild					 | ["event","Box","Set","Above","Child"]
eventBoxSetAboveChildPassive :: (EventBox) -> ReactiveFieldWrite IO (Bool)
eventBoxSetAboveChildPassive w = wrapMW (eventBoxSetAboveChild w)

-- @T: eventBoxSetVisibleWindow					 | ["event","Box","Set","Visible","Window"]
eventBoxSetVisibleWindowPassive :: (EventBox) -> ReactiveFieldWrite IO (Bool)
eventBoxSetVisibleWindowPassive w = wrapMW (eventBoxSetVisibleWindow w)

-- @A: eventBoxVisibleWindow
eventBoxVisibleWindowPassive :: EventBox -> ReactiveFieldReadWrite IO Bool
eventBoxVisibleWindowPassive w = passivePropertyNE w eventBoxVisibleWindow


-- @C: afterChildAttached
afterChildAttachedReactive :: HandleBoxClass self => self -> ReactiveFieldRead IO ()
afterChildAttachedReactive w = reactivePropertyH_ w afterChildAttached

-- @C: afterChildDetached
afterChildDetachedReactive :: HandleBoxClass self => self -> ReactiveFieldRead IO ()
afterChildDetachedReactive w = reactivePropertyH_ w afterChildDetached

-- @G: handleBoxGetHandlePosition					 | ["handle","Box","Get","Handle","Position"]
handleBoxGetHandlePositionPassive :: HandleBoxClass self => (self) -> ReactiveFieldRead IO (PositionType)
handleBoxGetHandlePositionPassive w = wrapMRPassive (handleBoxGetHandlePosition w)


-- @G: handleBoxGetShadowType					 | ["handle","Box","Get","Shadow","Type"]
handleBoxGetShadowTypePassive :: HandleBoxClass self => (self) -> ReactiveFieldRead IO (ShadowType)
handleBoxGetShadowTypePassive w = wrapMRPassive (handleBoxGetShadowType w)


-- @G: handleBoxGetSnapEdge					 | ["handle","Box","Get","Snap","Edge"]
handleBoxGetSnapEdgePassive :: HandleBoxClass self => (self) -> ReactiveFieldRead IO (PositionType)
handleBoxGetSnapEdgePassive w = wrapMRPassive (handleBoxGetSnapEdge w)


-- @A: handleBoxHandlePosition
handleBoxHandlePositionPassive :: HandleBoxClass self => (self) -> ReactiveFieldReadWrite IO (PositionType)
handleBoxHandlePositionPassive w = passivePropertyNE w handleBoxHandlePosition


-- @T: handleBoxSetHandlePosition					 | ["handle","Box","Set","Handle","Position"]
handleBoxSetHandlePositionReactive :: HandleBoxClass self => (self) -> ReactiveFieldWrite IO (PositionType)
handleBoxSetHandlePositionReactive w = wrapMW (handleBoxSetHandlePosition w)


-- @T: handleBoxSetShadowType					 | ["handle","Box","Set","Shadow","Type"]
handleBoxSetShadowTypeReactive :: HandleBoxClass self => (self) -> ReactiveFieldWrite IO (ShadowType)
handleBoxSetShadowTypeReactive w = wrapMW (handleBoxSetShadowType w)


-- @T: handleBoxSetSnapEdge					 | ["handle","Box","Set","Snap","Edge"]
handleBoxSetSnapEdgeReactive :: HandleBoxClass self => (self) -> ReactiveFieldWrite IO (PositionType)
handleBoxSetSnapEdgeReactive w = wrapMW (handleBoxSetSnapEdge w)


-- @A: handleBoxShadowType
handleBoxShadowTypePassive :: HandleBoxClass self => (self) -> ReactiveFieldReadWrite IO (ShadowType)
handleBoxShadowTypePassive w = passivePropertyNE w handleBoxShadowType


-- @A: handleBoxSnapEdge
handleBoxSnapEdgePassive :: HandleBoxClass self => (self) -> ReactiveFieldReadWrite IO (PositionType)
handleBoxSnapEdgePassive w = passivePropertyNE w handleBoxSnapEdge


-- @A: handleBoxSnapEdgeSet
handleBoxSnapEdgeSetPassive :: HandleBoxClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
handleBoxSnapEdgeSetPassive w = passivePropertyNE w handleBoxSnapEdgeSet


-- @C: onChildAttached
onChildAttachedReactive :: HandleBoxClass self => self -> ReactiveFieldRead IO ()
onChildAttachedReactive w = reactivePropertyH_ w onChildAttached

-- @C: onChildDetached
onChildDetachedReactive :: HandleBoxClass self => self -> ReactiveFieldRead IO ()
onChildDetachedReactive w = reactivePropertyH_ w onChildDetached

-- @G: sizeGroupGetIgnoreHidden					 | ["size","Group","Get","Ignore","Hidden"]
sizeGroupGetIgnoreHiddenPassive :: SizeGroupClass self => (self) -> ReactiveFieldRead IO (Bool)
sizeGroupGetIgnoreHiddenPassive w = wrapMRPassive (sizeGroupGetIgnoreHidden w)


-- @G: sizeGroupGetMode					 | ["size","Group","Get","Mode"]
sizeGroupGetModePassive :: SizeGroupClass self => (self) -> ReactiveFieldRead IO (SizeGroupMode)
sizeGroupGetModePassive w = wrapMRPassive (sizeGroupGetMode w)


-- @A: sizeGroupIgnoreHidden
sizeGroupIgnoreHiddenPassive :: SizeGroupClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
sizeGroupIgnoreHiddenPassive w = passivePropertyNE w sizeGroupIgnoreHidden


-- @A: sizeGroupMode
sizeGroupModePassive :: SizeGroupClass self => (self) -> ReactiveFieldReadWrite IO (SizeGroupMode)
sizeGroupModePassive w = passivePropertyNE w sizeGroupMode


-- @T: sizeGroupSetIgnoreHidden					 | ["size","Group","Set","Ignore","Hidden"]
sizeGroupSetIgnoreHiddenReactive :: SizeGroupClass self => (self) -> ReactiveFieldWrite IO (Bool)
sizeGroupSetIgnoreHiddenReactive w = wrapMW (sizeGroupSetIgnoreHidden w)


-- @T: sizeGroupSetMode					 | ["size","Group","Set","Mode"]
sizeGroupSetModeReactive :: SizeGroupClass self => (self) -> ReactiveFieldWrite IO (SizeGroupMode)
sizeGroupSetModeReactive w = wrapMW (sizeGroupSetMode w)


-- @G: tooltipsDataGet					 | ["tooltips","Data","Get"]
tooltipsDataGetPassive :: (WidgetClass w, GlibString string) => (w) -> ReactiveFieldRead IO ((Maybe (Tooltips, string, string)))
tooltipsDataGetPassive w = wrapMRPassive (tooltipsDataGet w)


-- @T: tooltipsSetDelay					 | ["tooltips","Set","Delay"]
tooltipsSetDelayReactive :: TooltipsClass self => (self) -> ReactiveFieldWrite IO (Int)
tooltipsSetDelayReactive w = wrapMW (tooltipsSetDelay w)


-- @T: tooltipsSetTip					 | ["tooltips","Set","Tip"]
-- TODO
-- @T: tooltipSetCustom					 | ["tooltip","Set","Custom"]
tooltipSetCustomReactive :: (TooltipClass self, WidgetClass widget) => (self) -> ReactiveFieldWrite IO (Maybe widget)
tooltipSetCustomReactive w = wrapMW (tooltipSetCustom w)


-- @T: tooltipSetIconFromGIcon					 | ["tooltip","Set","Icon","From","GIcon"]
-- TODO
-- @T: tooltipSetIconFromIconName					 | ["tooltip","Set","Icon","From","Icon","Name"]
-- TODO
-- @T: tooltipSetIconFromStock					 | ["tooltip","Set","Icon","From","Stock"]
-- TODO
-- @T: tooltipSetIcon					 | ["tooltip","Set","Icon"]
tooltipSetIconReactive :: TooltipClass self => (self) -> ReactiveFieldWrite IO (Maybe Pixbuf)
tooltipSetIconReactive w = wrapMW (tooltipSetIcon w)


-- @T: tooltipSetMarkup					 | ["tooltip","Set","Markup"]
tooltipSetMarkupReactive :: (TooltipClass self, GlibString markup) => (self) -> ReactiveFieldWrite IO (Maybe markup)
tooltipSetMarkupReactive w = wrapMW (tooltipSetMarkup w)


-- @T: tooltipSetText					 | ["tooltip","Set","Text"]
tooltipSetTextReactive :: (TooltipClass self, GlibString string) => (self) -> ReactiveFieldWrite IO (Maybe string)
tooltipSetTextReactive w = wrapMW (tooltipSetText w)


-- @T: tooltipSetTipArea					 | ["tooltip","Set","Tip","Area"]
tooltipSetTipAreaReactive :: TooltipClass self => (self) -> ReactiveFieldWrite IO (Rectangle)
tooltipSetTipAreaReactive w = wrapMW (tooltipSetTipArea w)


-- @G: viewportGetBinWindow					 | ["viewport","Get","Bin","Window"]
viewportGetBinWindowPassive :: ViewportClass self => (self) -> ReactiveFieldRead IO (DrawWindow)
viewportGetBinWindowPassive w = wrapMRPassive (viewportGetBinWindow w)


-- @G: viewportGetHAdjustment					 | ["viewport","Get","HAdjustment"]
viewportGetHAdjustmentPassive :: ViewportClass self => (self) -> ReactiveFieldRead IO (Adjustment)
viewportGetHAdjustmentPassive w = wrapMRPassive (viewportGetHAdjustment w)


-- @G: viewportGetShadowType					 | ["viewport","Get","Shadow","Type"]
viewportGetShadowTypePassive :: ViewportClass self => (self) -> ReactiveFieldRead IO (ShadowType)
viewportGetShadowTypePassive w = wrapMRPassive (viewportGetShadowType w)


-- @G: viewportGetVAdjustment					 | ["viewport","Get","VAdjustment"]
viewportGetVAdjustmentPassive :: ViewportClass self => (self) -> ReactiveFieldRead IO (Adjustment)
viewportGetVAdjustmentPassive w = wrapMRPassive (viewportGetVAdjustment w)


-- @G: viewportGetViewWindow					 | ["viewport","Get","View","Window"]
viewportGetViewWindowPassive :: ViewportClass self => (self) -> ReactiveFieldRead IO (DrawWindow)
viewportGetViewWindowPassive w = wrapMRPassive (viewportGetViewWindow w)


-- @A: viewportHAdjustment
viewportHAdjustmentPassive :: ViewportClass self => (self) -> ReactiveFieldReadWrite IO (Adjustment)
viewportHAdjustmentPassive w = passivePropertyNE w viewportHAdjustment


-- @T: viewportSetHAdjustment					 | ["viewport","Set","HAdjustment"]
viewportSetHAdjustmentReactive :: ViewportClass self => (self) -> ReactiveFieldWrite IO (Adjustment)
viewportSetHAdjustmentReactive w = wrapMW (viewportSetHAdjustment w)


-- @T: viewportSetShadowType					 | ["viewport","Set","Shadow","Type"]
viewportSetShadowTypeReactive :: ViewportClass self => (self) -> ReactiveFieldWrite IO (ShadowType)
viewportSetShadowTypeReactive w = wrapMW (viewportSetShadowType w)


-- @T: viewportSetVAdjustment					 | ["viewport","Set","VAdjustment"]
viewportSetVAdjustmentReactive :: ViewportClass self => (self) -> ReactiveFieldWrite IO (Adjustment)
viewportSetVAdjustmentReactive w = wrapMW (viewportSetVAdjustment w)


-- @A: viewportShadowType
viewportShadowTypePassive :: ViewportClass self => (self) -> ReactiveFieldReadWrite IO (ShadowType)
viewportShadowTypePassive w = passivePropertyNE w viewportShadowType


-- @A: viewportVAdjustment
viewportVAdjustmentPassive :: ViewportClass self => (self) -> ReactiveFieldReadWrite IO (Adjustment)
viewportVAdjustmentPassive w = passivePropertyNE w viewportVAdjustment


-- @A: cellEditableEditingCanceled
cellEditableEditingCanceledPassive :: CellEditableClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
cellEditableEditingCanceledPassive w = passivePropertyNE w cellEditableEditingCanceled


-- @S: cellEditableEditingDone
cellEditableEditingDoneReactive :: CellEditableClass self => self -> ReactiveFieldRead IO ()
cellEditableEditingDoneReactive = (`reactiveSignalIO` cellEditableEditingDone)


-- @S: cellEditableRemoveWidget
cellEditableRemoveWidgetReactive :: CellEditableClass self => self -> ReactiveFieldRead IO ()
cellEditableRemoveWidgetReactive = (`reactiveSignalIO` cellEditableRemoveWidget)


-- @G: cellLayoutGetCells					 | ["cell","Layout","Get","Cells"]
cellLayoutGetCellsPassive :: CellLayoutClass self => (self) -> ReactiveFieldRead IO ([CellRenderer])
cellLayoutGetCellsPassive w = wrapMRPassive (cellLayoutGetCells w)


-- @T: cellLayoutSetAttributeFunc					 | ["cell","Layout","Set","Attribute","Func"]
-- TODO
-- @T: cellLayoutSetAttributes					 | ["cell","Layout","Set","Attributes"]
-- TODO
-- @S: accelCleared
-- TODO
-- @S: accelEdited
-- TODO
-- @A: cellRendererAccelAccelKey
cellRendererAccelAccelKeyPassive :: CellRendererAccelClass self => (self) -> ReactiveFieldReadWrite IO (Int)
cellRendererAccelAccelKeyPassive w = passivePropertyNE w cellRendererAccelAccelKey


-- @A: cellRendererAccelAccelMode
cellRendererAccelAccelModePassive :: CellRendererAccelClass self => (self) -> ReactiveFieldReadWrite IO (CellRendererAccelMode)
cellRendererAccelAccelModePassive w = passivePropertyNE w cellRendererAccelAccelMode


-- @A: cellRendererAccelAccelMods
cellRendererAccelAccelModsPassive :: CellRendererAccelClass self => (self) -> ReactiveFieldReadWrite IO ([Modifier])
cellRendererAccelAccelModsPassive w = passivePropertyNE w cellRendererAccelAccelMods


-- @A: cellRendererAccelKeycode
cellRendererAccelKeycodePassive :: CellRendererAccelClass self => (self) -> ReactiveFieldReadWrite IO (Int)
cellRendererAccelKeycodePassive w = passivePropertyNE w cellRendererAccelKeycode


-- @C: afterEditingCanceled
afterEditingCanceledReactive :: CellRendererClass self => self -> ReactiveFieldRead IO ()
afterEditingCanceledReactive w = reactivePropertyH_ w afterEditingCanceled

-- @C: afterEditingStarted
-- TODO
-- @A: cellBackgroundColor
cellBackgroundColorPassive :: CellRendererClass self => (self) -> ReactiveFieldReadWrite IO (Color)
cellBackgroundColorPassive w = passivePropertyNE w cellBackgroundColor


-- @A: cellBackgroundSet
cellBackgroundSetPassive :: CellRendererClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
cellBackgroundSetPassive w = passivePropertyNE w cellBackgroundSet


-- @A: cellHeight
cellHeightPassive :: CellRendererClass self => (self) -> ReactiveFieldReadWrite IO (Int)
cellHeightPassive w = passivePropertyNE w cellHeight


-- @A: cellIsExpanded
cellIsExpandedPassive :: CellRendererClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
cellIsExpandedPassive w = passivePropertyNE w cellIsExpanded


-- @A: cellIsExpander
cellIsExpanderPassive :: CellRendererClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
cellIsExpanderPassive w = passivePropertyNE w cellIsExpander


-- @A: cellMode
cellModePassive :: CellRendererClass self => (self) -> ReactiveFieldReadWrite IO (CellRendererMode)
cellModePassive w = passivePropertyNE w cellMode


-- @G: cellRendererGetFixedSize					 | ["cell","Renderer","Get","Fixed","Size"]
cellRendererGetFixedSizePassive :: CellRendererClass self => (self) -> ReactiveFieldRead IO ((Int, Int))
cellRendererGetFixedSizePassive w = wrapMRPassive (cellRendererGetFixedSize w)


-- @T: cellRendererSetFixedSize					 | ["cell","Renderer","Set","Fixed","Size"]
-- TODO
-- @A: cellSensitive
cellSensitivePassive :: CellRendererClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
cellSensitivePassive w = passivePropertyNE w cellSensitive


-- @A: cellVisible
cellVisiblePassive :: CellRendererClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
cellVisiblePassive w = passivePropertyNE w cellVisible


-- @A: cellWidth
cellWidthPassive :: CellRendererClass self => (self) -> ReactiveFieldReadWrite IO (Int)
cellWidthPassive w = passivePropertyNE w cellWidth


-- @A: cellXAlign
cellXAlignPassive :: CellRendererClass self => (self) -> ReactiveFieldReadWrite IO (Float)
cellXAlignPassive w = passivePropertyNE w cellXAlign


-- @A: cellXPad
cellXPadPassive :: CellRendererClass self => (self) -> ReactiveFieldReadWrite IO (Int)
cellXPadPassive w = passivePropertyNE w cellXPad


-- @A: cellYAlign
cellYAlignPassive :: CellRendererClass self => (self) -> ReactiveFieldReadWrite IO (Float)
cellYAlignPassive w = passivePropertyNE w cellYAlign


-- @A: cellYPad
cellYPadPassive :: CellRendererClass self => (self) -> ReactiveFieldReadWrite IO (Int)
cellYPadPassive w = passivePropertyNE w cellYPad


-- @A: cellComboHasEntry
cellComboHasEntryPassive :: CellRendererComboClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
cellComboHasEntryPassive w = passivePropertyNE w cellComboHasEntry


-- @S: editingCanceled
editingCanceledReactive :: CellRendererClass self => self -> ReactiveFieldRead IO ()
editingCanceledReactive = (`reactiveSignalIO` editingCanceled)


-- @S: editingStarted
-- TODO
-- @C: onEditingCanceled
onEditingCanceledReactive :: CellRendererClass self => self -> ReactiveFieldRead IO ()
onEditingCanceledReactive w = reactivePropertyH_ w onEditingCanceled

-- @C: onEditingStarted
-- TODO
-- @A: cellPixbufExpanderClosed
cellPixbufExpanderClosedPassive :: CellRendererPixbufClass self => (self) -> ReactiveFieldReadWrite IO (Pixbuf)
cellPixbufExpanderClosedPassive w = passivePropertyNE w cellPixbufExpanderClosed


-- @A: cellPixbufExpanderOpen
cellPixbufExpanderOpenPassive :: CellRendererPixbufClass self => (self) -> ReactiveFieldReadWrite IO (Pixbuf)
cellPixbufExpanderOpenPassive w = passivePropertyNE w cellPixbufExpanderOpen


-- @A: cellPixbufFollowState
cellPixbufFollowStatePassive :: CellRendererPixbufClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
cellPixbufFollowStatePassive w = passivePropertyNE w cellPixbufFollowState


-- @A: cellPixbuf
cellPixbufPassive :: CellRendererPixbufClass self => (self) -> ReactiveFieldReadWrite IO (Pixbuf)
cellPixbufPassive w = passivePropertyNE w cellPixbuf


-- @A: cellPixbufIconName
cellPixbufIconNamePassive :: (CellRendererPixbufClass self, GlibString string) => (self) -> ReactiveFieldReadWrite IO (string)
cellPixbufIconNamePassive w = passivePropertyNE w cellPixbufIconName


-- @A: cellPixbufStockDetail
cellPixbufStockDetailPassive :: (CellRendererPixbufClass self, GlibString string) => (self) -> ReactiveFieldReadWrite IO (string)
cellPixbufStockDetailPassive w = passivePropertyNE w cellPixbufStockDetail


-- @A: cellPixbufStockId
cellPixbufStockIdPassive :: (CellRendererPixbufClass self, GlibString string) => (self) -> ReactiveFieldReadWrite IO (string)
cellPixbufStockIdPassive w = passivePropertyNE w cellPixbufStockId


-- @A: cellPixbufStockSize
cellPixbufStockSizePassive :: CellRendererPixbufClass self => (self) -> ReactiveFieldReadWrite IO (Int)
cellPixbufStockSizePassive w = passivePropertyNE w cellPixbufStockSize


-- @A: cellProgressText
cellProgressTextPassive :: (CellRendererProgressClass self, GlibString string) => (self) -> ReactiveFieldReadWrite IO ((Maybe string))
cellProgressTextPassive w = passivePropertyNE w cellProgressText


-- @A: cellProgressValue
cellProgressValuePassive :: CellRendererProgressClass self => (self) -> ReactiveFieldReadWrite IO (Int)
cellProgressValuePassive w = passivePropertyNE w cellProgressValue


-- @A: cellRendererSpinAdjustment
cellRendererSpinAdjustmentPassive :: CellRendererSpinClass self => (self) -> ReactiveFieldReadWrite IO (Adjustment)
cellRendererSpinAdjustmentPassive w = passivePropertyNE w cellRendererSpinAdjustment


-- @A: cellRendererSpinClimbRate
cellRendererSpinClimbRatePassive :: CellRendererSpinClass self => (self) -> ReactiveFieldReadWrite IO (Double)
cellRendererSpinClimbRatePassive w = passivePropertyNE w cellRendererSpinClimbRate


-- @A: cellRendererSpinDigits
cellRendererSpinDigitsPassive :: CellRendererSpinClass self => (self) -> ReactiveFieldReadWrite IO (Int)
cellRendererSpinDigitsPassive w = passivePropertyNE w cellRendererSpinDigits


-- @A: cellRendererSpinnerActive
cellRendererSpinnerActivePassive :: CellRendererSpinnerClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
cellRendererSpinnerActivePassive w = passivePropertyNE w cellRendererSpinnerActive


-- @A: cellRendererSpinnerPulse
cellRendererSpinnerPulsePassive :: CellRendererSpinnerClass self => (self) -> ReactiveFieldReadWrite IO (Int)
cellRendererSpinnerPulsePassive w = passivePropertyNE w cellRendererSpinnerPulse


-- @A: cellRendererSpinnerSize
cellRendererSpinnerSizePassive :: CellRendererSpinnerClass self => (self) -> ReactiveFieldReadWrite IO (IconSize)
cellRendererSpinnerSizePassive w = passivePropertyNE w cellRendererSpinnerSize


-- @C: afterEdited
-- TODO
-- @T: cellRendererTextSetFixedHeightFromFont					 | ["cell","Renderer","Text","Set","Fixed","Height","From","Font"]
cellRendererTextSetFixedHeightFromFontReactive :: CellRendererTextClass self => (self) -> ReactiveFieldWrite IO (Int)
cellRendererTextSetFixedHeightFromFontReactive w = wrapMW (cellRendererTextSetFixedHeightFromFont w)


-- @A: cellTextAlignment
cellTextAlignmentPassive :: CellRendererTextClass self => (self) -> ReactiveFieldReadWrite IO (LayoutAlignment)
cellTextAlignmentPassive w = passivePropertyNE w cellTextAlignment


-- @A: cellTextBackgroundColor
cellTextBackgroundColorPassive :: CellRendererClass self => (self) -> ReactiveFieldReadWrite IO (Color)
cellTextBackgroundColorPassive w = passivePropertyNE w cellTextBackgroundColor


-- @A: cellTextBackgroundSet
cellTextBackgroundSetPassive :: CellRendererClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
cellTextBackgroundSetPassive w = passivePropertyNE w cellTextBackgroundSet


-- @A: cellTextEditable
cellTextEditablePassive :: CellRendererTextClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
cellTextEditablePassive w = passivePropertyNE w cellTextEditable


-- @A: cellTextEditableSet
cellTextEditableSetPassive :: CellRendererTextClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
cellTextEditableSetPassive w = passivePropertyNE w cellTextEditableSet


-- @A: cellTextEllipsize
cellTextEllipsizePassive :: CellRendererTextClass self => (self) -> ReactiveFieldReadWrite IO (EllipsizeMode)
cellTextEllipsizePassive w = passivePropertyNE w cellTextEllipsize


-- @A: cellTextEllipsizeSet
cellTextEllipsizeSetPassive :: CellRendererTextClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
cellTextEllipsizeSetPassive w = passivePropertyNE w cellTextEllipsizeSet


-- @A: cellTextFamily
cellTextFamilyPassive :: (CellRendererTextClass self, GlibString string) => (self) -> ReactiveFieldReadWrite IO (string)
cellTextFamilyPassive w = passivePropertyNE w cellTextFamily


-- @A: cellTextFamilySet
cellTextFamilySetPassive :: CellRendererTextClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
cellTextFamilySetPassive w = passivePropertyNE w cellTextFamilySet


-- @A: cellTextFontDesc
cellTextFontDescPassive :: CellRendererTextClass self => (self) -> ReactiveFieldReadWrite IO (FontDescription)
cellTextFontDescPassive w = passivePropertyNE w cellTextFontDesc


-- @A: cellTextFont
cellTextFontPassive :: (CellRendererTextClass self, GlibString string) => (self) -> ReactiveFieldReadWrite IO (string)
cellTextFontPassive w = passivePropertyNE w cellTextFont


-- @A: cellTextForegroundColor
cellTextForegroundColorPassive :: CellRendererClass self => (self) -> ReactiveFieldReadWrite IO (Color)
cellTextForegroundColorPassive w = passivePropertyNE w cellTextForegroundColor


-- @A: cellTextForegroundSet
cellTextForegroundSetPassive :: CellRendererClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
cellTextForegroundSetPassive w = passivePropertyNE w cellTextForegroundSet


-- @A: cellText
cellTextPassive :: (CellRendererTextClass cr, GlibString string) => (cr) -> ReactiveFieldReadWrite IO (string)
cellTextPassive w = passivePropertyNE w cellText


-- @A: cellTextLanguage
cellTextLanguagePassive :: (CellRendererTextClass self, GlibString string) => (self) -> ReactiveFieldReadWrite IO ((Maybe string))
cellTextLanguagePassive w = passivePropertyNE w cellTextLanguage


-- @A: cellTextLanguageSet
cellTextLanguageSetPassive :: CellRendererTextClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
cellTextLanguageSetPassive w = passivePropertyNE w cellTextLanguageSet


-- @A: cellTextRise
cellTextRisePassive :: CellRendererTextClass self => (self) -> ReactiveFieldReadWrite IO (Int)
cellTextRisePassive w = passivePropertyNE w cellTextRise


-- @A: cellTextRiseSet
cellTextRiseSetPassive :: CellRendererTextClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
cellTextRiseSetPassive w = passivePropertyNE w cellTextRiseSet


-- @A: cellTextScale
cellTextScalePassive :: CellRendererTextClass self => (self) -> ReactiveFieldReadWrite IO (Double)
cellTextScalePassive w = passivePropertyNE w cellTextScale


-- @A: cellTextScaleSet
cellTextScaleSetPassive :: CellRendererTextClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
cellTextScaleSetPassive w = passivePropertyNE w cellTextScaleSet


-- @A: cellTextSingleParagraphMode
cellTextSingleParagraphModePassive :: CellRendererTextClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
cellTextSingleParagraphModePassive w = passivePropertyNE w cellTextSingleParagraphMode


-- @A: cellTextSize
cellTextSizePassive :: CellRendererTextClass self => (self) -> ReactiveFieldReadWrite IO (Double)
cellTextSizePassive w = passivePropertyNE w cellTextSize


-- @A: cellTextSizePoints
cellTextSizePointsPassive :: CellRendererTextClass self => (self) -> ReactiveFieldReadWrite IO (Double)
cellTextSizePointsPassive w = passivePropertyNE w cellTextSizePoints


-- @A: cellTextSizeSet
cellTextSizeSetPassive :: CellRendererTextClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
cellTextSizeSetPassive w = passivePropertyNE w cellTextSizeSet


-- @A: cellTextStretch
cellTextStretchPassive :: CellRendererTextClass self => (self) -> ReactiveFieldReadWrite IO (Stretch)
cellTextStretchPassive w = passivePropertyNE w cellTextStretch


-- @A: cellTextStretchSet
cellTextStretchSetPassive :: CellRendererTextClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
cellTextStretchSetPassive w = passivePropertyNE w cellTextStretchSet


-- @A: cellTextStrikethrough
cellTextStrikethroughPassive :: CellRendererTextClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
cellTextStrikethroughPassive w = passivePropertyNE w cellTextStrikethrough


-- @A: cellTextStrikethroughSet
cellTextStrikethroughSetPassive :: CellRendererTextClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
cellTextStrikethroughSetPassive w = passivePropertyNE w cellTextStrikethroughSet


-- @A: cellTextStyle
cellTextStylePassive :: CellRendererTextClass self => (self) -> ReactiveFieldReadWrite IO (FontStyle)
cellTextStylePassive w = passivePropertyNE w cellTextStyle


-- @A: cellTextStyleSet
cellTextStyleSetPassive :: CellRendererTextClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
cellTextStyleSetPassive w = passivePropertyNE w cellTextStyleSet


-- @A: cellTextUnderline
cellTextUnderlinePassive :: CellRendererTextClass self => (self) -> ReactiveFieldReadWrite IO (Underline)
cellTextUnderlinePassive w = passivePropertyNE w cellTextUnderline


-- @A: cellTextUnderlineSet
cellTextUnderlineSetPassive :: CellRendererTextClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
cellTextUnderlineSetPassive w = passivePropertyNE w cellTextUnderlineSet


-- @A: cellTextVariant
cellTextVariantPassive :: CellRendererTextClass self => (self) -> ReactiveFieldReadWrite IO (Variant)
cellTextVariantPassive w = passivePropertyNE w cellTextVariant


-- @A: cellTextVariantSet
cellTextVariantSetPassive :: CellRendererTextClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
cellTextVariantSetPassive w = passivePropertyNE w cellTextVariantSet


-- @A: cellTextWeight
cellTextWeightPassive :: CellRendererTextClass self => (self) -> ReactiveFieldReadWrite IO (Int)
cellTextWeightPassive w = passivePropertyNE w cellTextWeight


-- @A: cellTextWeightSet
cellTextWeightSetPassive :: CellRendererTextClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
cellTextWeightSetPassive w = passivePropertyNE w cellTextWeightSet


-- @A: cellTextWidthChars
cellTextWidthCharsPassive :: CellRendererTextClass self => (self) -> ReactiveFieldReadWrite IO (Int)
cellTextWidthCharsPassive w = passivePropertyNE w cellTextWidthChars


-- @A: cellTextWrapMode
cellTextWrapModePassive :: CellRendererTextClass self => (self) -> ReactiveFieldReadWrite IO (LayoutWrapMode)
cellTextWrapModePassive w = passivePropertyNE w cellTextWrapMode


-- @A: cellTextWrapWidth
cellTextWrapWidthPassive :: CellRendererTextClass self => (self) -> ReactiveFieldReadWrite IO (Int)
cellTextWrapWidthPassive w = passivePropertyNE w cellTextWrapWidth


-- @S: edited
-- TODO
-- @C: onEdited
-- TODO
-- @C: afterCellToggled
-- TODO
-- @G: cellRendererToggleGetActive					 | ["cell","Renderer","Toggle","Get","Active"]
cellRendererToggleGetActivePassive :: CellRendererToggleClass self => (self) -> ReactiveFieldRead IO (Bool)
cellRendererToggleGetActivePassive w = wrapMRPassive (cellRendererToggleGetActive w)


-- @G: cellRendererToggleGetRadio					 | ["cell","Renderer","Toggle","Get","Radio"]
cellRendererToggleGetRadioPassive :: CellRendererToggleClass self => (self) -> ReactiveFieldRead IO (Bool)
cellRendererToggleGetRadioPassive w = wrapMRPassive (cellRendererToggleGetRadio w)


-- @T: cellRendererToggleSetActive					 | ["cell","Renderer","Toggle","Set","Active"]
cellRendererToggleSetActiveReactive :: CellRendererToggleClass self => (self) -> ReactiveFieldWrite IO (Bool)
cellRendererToggleSetActiveReactive w = wrapMW (cellRendererToggleSetActive w)


-- @T: cellRendererToggleSetRadio					 | ["cell","Renderer","Toggle","Set","Radio"]
cellRendererToggleSetRadioReactive :: CellRendererToggleClass self => (self) -> ReactiveFieldWrite IO (Bool)
cellRendererToggleSetRadioReactive w = wrapMW (cellRendererToggleSetRadio w)


-- @A: cellToggleActivatable
cellToggleActivatablePassive :: CellRendererToggleClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
cellToggleActivatablePassive w = passivePropertyNE w cellToggleActivatable


-- @A: cellToggleActive
cellToggleActivePassive :: CellRendererToggleClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
cellToggleActivePassive w = passivePropertyNE w cellToggleActive


-- @S: cellToggled
-- TODO
-- @A: cellToggleInconsistent
cellToggleInconsistentPassive :: CellRendererToggleClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
cellToggleInconsistentPassive w = passivePropertyNE w cellToggleInconsistent


-- @A: cellToggleIndicatorSize
cellToggleIndicatorSizePassive :: CellRendererToggleClass self => (self) -> ReactiveFieldReadWrite IO (Int)
cellToggleIndicatorSizePassive w = passivePropertyNE w cellToggleIndicatorSize


-- @A: cellToggleRadio
cellToggleRadioPassive :: CellRendererToggleClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
cellToggleRadioPassive w = passivePropertyNE w cellToggleRadio


-- @C: onCellToggled
-- TODO
-- @G: cellViewGetCellRenderers					 | ["cell","View","Get","Cell","Renderers"]
cellViewGetCellRenderersPassive :: CellViewClass self => (self) -> ReactiveFieldRead IO ([CellRenderer])
cellViewGetCellRenderersPassive w = wrapMRPassive (cellViewGetCellRenderers w)


-- @G: cellViewGetSizeOfRow					 | ["cell","View","Get","Size","Of","Row"]
-- TODO
-- @T: cellViewSetBackgroundColor					 | ["cell","View","Set","Background","Color"]
cellViewSetBackgroundColorReactive :: CellViewClass self => (self) -> ReactiveFieldWrite IO (Color)
cellViewSetBackgroundColorReactive w = wrapMW (cellViewSetBackgroundColor w)


-- @T: cellViewSetModel					 | ["cell","View","Set","Model"]
cellViewSetModelReactive :: (CellViewClass self, TreeModelClass model) => (self) -> ReactiveFieldWrite IO (Maybe model)
cellViewSetModelReactive w = wrapMW (cellViewSetModel w)


-- @G: customStoreGetPrivate					 | ["custom","Store","Get","Private"]
-- TODO
-- @G: customStoreGetRow					 | ["custom","Store","Get","Row"]
-- TODO
-- @G: customStoreGetStamp					 | ["custom","Store","Get","Stamp"]
customStoreGetStampPassive :: (CustomStore private row) -> ReactiveFieldRead IO (C.CInt)
customStoreGetStampPassive w = wrapMRPassive (customStoreGetStamp w)


-- @T: customStoreSetColumn					 | ["custom","Store","Set","Column"]
-- TODO
-- @G: treeModelGetRow					 | ["tree","Model","Get","Row"]
-- TODO
-- @T: treeModelSetColumn					 | ["tree","Model","Set","Column"]
-- TODO
-- @A: iconViewColumns
iconViewColumnsPassive :: IconViewClass self => (self) -> ReactiveFieldReadWrite IO (Int)
iconViewColumnsPassive w = passivePropertyNE w iconViewColumns


-- @A: iconViewColumnSpacing
iconViewColumnSpacingPassive :: IconViewClass self => (self) -> ReactiveFieldReadWrite IO (Int)
iconViewColumnSpacingPassive w = passivePropertyNE w iconViewColumnSpacing


-- @G: iconViewGetColumns					 | ["icon","View","Get","Columns"]
iconViewGetColumnsPassive :: IconViewClass self => (self) -> ReactiveFieldRead IO (Int)
iconViewGetColumnsPassive w = wrapMRPassive (iconViewGetColumns w)


-- @G: iconViewGetColumnSpacing					 | ["icon","View","Get","Column","Spacing"]
iconViewGetColumnSpacingPassive :: IconViewClass self => (self) -> ReactiveFieldRead IO (Int)
iconViewGetColumnSpacingPassive w = wrapMRPassive (iconViewGetColumnSpacing w)


-- @G: iconViewGetCursor					 | ["icon","View","Get","Cursor"]
iconViewGetCursorPassive :: IconViewClass self => (self) -> ReactiveFieldRead IO ((TreePath, Maybe CellRenderer))
iconViewGetCursorPassive w = wrapMRPassive (iconViewGetCursor w)


-- @G: iconViewGetItemAtPos					 | ["icon","View","Get","Item","At","Pos"]
-- TODO
-- @G: iconViewGetItemColumn					 | ["icon","View","Get","Item","Column"]
-- TODO
-- @G: iconViewGetItemRow					 | ["icon","View","Get","Item","Row"]
-- TODO
-- @G: iconViewGetItemWidth					 | ["icon","View","Get","Item","Width"]
iconViewGetItemWidthPassive :: IconViewClass self => (self) -> ReactiveFieldRead IO (Int)
iconViewGetItemWidthPassive w = wrapMRPassive (iconViewGetItemWidth w)


-- @G: iconViewGetMargin					 | ["icon","View","Get","Margin"]
iconViewGetMarginPassive :: IconViewClass self => (self) -> ReactiveFieldRead IO (Int)
iconViewGetMarginPassive w = wrapMRPassive (iconViewGetMargin w)


-- @G: iconViewGetMarkupColumn					 | ["icon","View","Get","Markup","Column"]
iconViewGetMarkupColumnPassive :: (IconViewClass self, GlibString markup) => (self) -> ReactiveFieldRead IO ((ColumnId row markup))
iconViewGetMarkupColumnPassive w = wrapMRPassive (iconViewGetMarkupColumn w)


-- @G: iconViewGetModel					 | ["icon","View","Get","Model"]
iconViewGetModelPassive :: IconViewClass self => (self) -> ReactiveFieldRead IO ((Maybe TreeModel))
iconViewGetModelPassive w = wrapMRPassive (iconViewGetModel w)


-- @G: iconViewGetOrientation					 | ["icon","View","Get","Orientation"]
iconViewGetOrientationPassive :: IconViewClass self => (self) -> ReactiveFieldRead IO (Orientation)
iconViewGetOrientationPassive w = wrapMRPassive (iconViewGetOrientation w)


-- @G: iconViewGetPathAtPos					 | ["icon","View","Get","Path","At","Pos"]
-- TODO
-- @G: iconViewGetPixbufColumn					 | ["icon","View","Get","Pixbuf","Column"]
iconViewGetPixbufColumnPassive :: IconViewClass self => (self) -> ReactiveFieldRead IO ((ColumnId row Pixbuf))
iconViewGetPixbufColumnPassive w = wrapMRPassive (iconViewGetPixbufColumn w)


-- @G: iconViewGetReorderable					 | ["icon","View","Get","Reorderable"]
iconViewGetReorderablePassive :: IconViewClass self => (self) -> ReactiveFieldRead IO (Bool)
iconViewGetReorderablePassive w = wrapMRPassive (iconViewGetReorderable w)


-- @G: iconViewGetRowSpacing					 | ["icon","View","Get","Row","Spacing"]
iconViewGetRowSpacingPassive :: IconViewClass self => (self) -> ReactiveFieldRead IO (Int)
iconViewGetRowSpacingPassive w = wrapMRPassive (iconViewGetRowSpacing w)


-- @G: iconViewGetSelectedItems					 | ["icon","View","Get","Selected","Items"]
iconViewGetSelectedItemsPassive :: IconViewClass self => (self) -> ReactiveFieldRead IO ([TreePath])
iconViewGetSelectedItemsPassive w = wrapMRPassive (iconViewGetSelectedItems w)


-- @G: iconViewGetSelectionMode					 | ["icon","View","Get","Selection","Mode"]
iconViewGetSelectionModePassive :: IconViewClass self => (self) -> ReactiveFieldRead IO (SelectionMode)
iconViewGetSelectionModePassive w = wrapMRPassive (iconViewGetSelectionMode w)


-- @G: iconViewGetSpacing					 | ["icon","View","Get","Spacing"]
iconViewGetSpacingPassive :: IconViewClass self => (self) -> ReactiveFieldRead IO (Int)
iconViewGetSpacingPassive w = wrapMRPassive (iconViewGetSpacing w)


-- @G: iconViewGetTextColumn					 | ["icon","View","Get","Text","Column"]
iconViewGetTextColumnPassive :: (IconViewClass self, GlibString string) => (self) -> ReactiveFieldRead IO ((ColumnId row string))
iconViewGetTextColumnPassive w = wrapMRPassive (iconViewGetTextColumn w)


-- @G: iconViewGetVisibleRange					 | ["icon","View","Get","Visible","Range"]
iconViewGetVisibleRangePassive :: IconViewClass self => (self) -> ReactiveFieldRead IO ((Maybe (TreePath, TreePath)))
iconViewGetVisibleRangePassive w = wrapMRPassive (iconViewGetVisibleRange w)


-- @A: iconViewItemOrientation
iconViewItemOrientationPassive :: IconViewClass self => (self) -> ReactiveFieldReadWrite IO (Orientation)
iconViewItemOrientationPassive w = passivePropertyNE w iconViewItemOrientation


-- @A: iconViewItemWidth
iconViewItemWidthPassive :: IconViewClass self => (self) -> ReactiveFieldReadWrite IO (Int)
iconViewItemWidthPassive w = passivePropertyNE w iconViewItemWidth


-- @A: iconViewMargin
iconViewMarginPassive :: IconViewClass self => (self) -> ReactiveFieldReadWrite IO (Int)
iconViewMarginPassive w = passivePropertyNE w iconViewMargin


-- @A: iconViewMarkupColumn
iconViewMarkupColumnPassive :: (IconViewClass self, GlibString markup) => (self) -> ReactiveFieldReadWrite IO ((ColumnId row markup))
iconViewMarkupColumnPassive w = passivePropertyNE w iconViewMarkupColumn


-- @A: iconViewOrientation
iconViewOrientationPassive :: IconViewClass self => (self) -> ReactiveFieldReadWrite IO (Orientation)
iconViewOrientationPassive w = passivePropertyNE w iconViewOrientation


-- @A: iconViewPixbufColumn
iconViewPixbufColumnPassive :: IconViewClass self => (self) -> ReactiveFieldReadWrite IO ((ColumnId row Pixbuf))
iconViewPixbufColumnPassive w = passivePropertyNE w iconViewPixbufColumn


-- @A: iconViewReorderable
iconViewReorderablePassive :: IconViewClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
iconViewReorderablePassive w = passivePropertyNE w iconViewReorderable


-- @A: iconViewRowSpacing
iconViewRowSpacingPassive :: IconViewClass self => (self) -> ReactiveFieldReadWrite IO (Int)
iconViewRowSpacingPassive w = passivePropertyNE w iconViewRowSpacing


-- @A: iconViewSelectionMode
iconViewSelectionModePassive :: IconViewClass self => (self) -> ReactiveFieldReadWrite IO (SelectionMode)
iconViewSelectionModePassive w = passivePropertyNE w iconViewSelectionMode


-- @T: iconViewSetColumns					 | ["icon","View","Set","Columns"]
iconViewSetColumnsReactive :: IconViewClass self => (self) -> ReactiveFieldWrite IO (Int)
iconViewSetColumnsReactive w = wrapMW (iconViewSetColumns w)


-- @T: iconViewSetColumnSpacing					 | ["icon","View","Set","Column","Spacing"]
iconViewSetColumnSpacingReactive :: IconViewClass self => (self) -> ReactiveFieldWrite IO (Int)
iconViewSetColumnSpacingReactive w = wrapMW (iconViewSetColumnSpacing w)


-- @T: iconViewSetCursor					 | ["icon","View","Set","Cursor"]
-- TODO
-- @T: iconViewSetItemWidth					 | ["icon","View","Set","Item","Width"]
iconViewSetItemWidthReactive :: IconViewClass self => (self) -> ReactiveFieldWrite IO (Int)
iconViewSetItemWidthReactive w = wrapMW (iconViewSetItemWidth w)


-- @T: iconViewSetMargin					 | ["icon","View","Set","Margin"]
iconViewSetMarginReactive :: IconViewClass self => (self) -> ReactiveFieldWrite IO (Int)
iconViewSetMarginReactive w = wrapMW (iconViewSetMargin w)


-- @T: iconViewSetMarkupColumn					 | ["icon","View","Set","Markup","Column"]
iconViewSetMarkupColumnReactive :: (IconViewClass self, GlibString markup) => (self) -> ReactiveFieldWrite IO (ColumnId row markup)
iconViewSetMarkupColumnReactive w = wrapMW (iconViewSetMarkupColumn w)


-- @T: iconViewSetModel					 | ["icon","View","Set","Model"]
iconViewSetModelReactive :: (IconViewClass self, TreeModelClass model) => (self) -> ReactiveFieldWrite IO (Maybe model)
iconViewSetModelReactive w = wrapMW (iconViewSetModel w)


-- @T: iconViewSetOrientation					 | ["icon","View","Set","Orientation"]
iconViewSetOrientationReactive :: IconViewClass self => (self) -> ReactiveFieldWrite IO (Orientation)
iconViewSetOrientationReactive w = wrapMW (iconViewSetOrientation w)


-- @T: iconViewSetPixbufColumn					 | ["icon","View","Set","Pixbuf","Column"]
iconViewSetPixbufColumnReactive :: IconViewClass self => (self) -> ReactiveFieldWrite IO (ColumnId row Pixbuf)
iconViewSetPixbufColumnReactive w = wrapMW (iconViewSetPixbufColumn w)


-- @T: iconViewSetReorderable					 | ["icon","View","Set","Reorderable"]
iconViewSetReorderableReactive :: IconViewClass self => (self) -> ReactiveFieldWrite IO (Bool)
iconViewSetReorderableReactive w = wrapMW (iconViewSetReorderable w)


-- @T: iconViewSetRowSpacing					 | ["icon","View","Set","Row","Spacing"]
iconViewSetRowSpacingReactive :: IconViewClass self => (self) -> ReactiveFieldWrite IO (Int)
iconViewSetRowSpacingReactive w = wrapMW (iconViewSetRowSpacing w)


-- @T: iconViewSetSelectionMode					 | ["icon","View","Set","Selection","Mode"]
iconViewSetSelectionModeReactive :: IconViewClass self => (self) -> ReactiveFieldWrite IO (SelectionMode)
iconViewSetSelectionModeReactive w = wrapMW (iconViewSetSelectionMode w)


-- @T: iconViewSetSpacing					 | ["icon","View","Set","Spacing"]
iconViewSetSpacingReactive :: IconViewClass self => (self) -> ReactiveFieldWrite IO (Int)
iconViewSetSpacingReactive w = wrapMW (iconViewSetSpacing w)


-- @T: iconViewSetTextColumn					 | ["icon","View","Set","Text","Column"]
iconViewSetTextColumnReactive :: (IconViewClass self, GlibString string) => (self) -> ReactiveFieldWrite IO (ColumnId row string)
iconViewSetTextColumnReactive w = wrapMW (iconViewSetTextColumn w)


-- @A: iconViewSpacing
iconViewSpacingPassive :: IconViewClass self => (self) -> ReactiveFieldReadWrite IO (Int)
iconViewSpacingPassive w = passivePropertyNE w iconViewSpacing


-- @A: iconViewTextColumn
iconViewTextColumnPassive :: (IconViewClass self, GlibString string) => (self) -> ReactiveFieldReadWrite IO ((ColumnId row string))
iconViewTextColumnPassive w = passivePropertyNE w iconViewTextColumn


-- @S: itemActivated
-- TODO
-- @S: selectionChanged
selectionChangedReactive :: IconViewClass self => self -> ReactiveFieldRead IO ()
selectionChangedReactive = (`reactiveSignalIO` selectionChanged)


-- @S: setIconViewScrollAdjustments
-- TODO
-- @G: listStoreGetSize					 | ["list","Store","Get","Size"]
listStoreGetSizePassive :: (ListStore a) -> ReactiveFieldRead IO (Int)
listStoreGetSizePassive w = wrapMRPassive (listStoreGetSize w)


-- @G: listStoreGetValue					 | ["list","Store","Get","Value"]
-- TODO
-- @G: listStoreSafeGetValue					 | ["list","Store","Safe","Get","Value"]
-- TODO
-- @T: listStoreSetValue					 | ["list","Store","Set","Value"]
-- TODO
-- @G: treeGetRowDragData					 | ["tree","Get","Row","Drag","Data"]
-- TODO
-- @T: treeSetRowDragData					 | ["tree","Set","Row","Drag","Data"]
-- TODO
-- @G: treeModelFilterGetModel					 | ["tree","Model","Filter","Get","Model"]
treeModelFilterGetModelPassive :: TreeModelFilterClass self => (self) -> ReactiveFieldRead IO ((Maybe TreeModel))
treeModelFilterGetModelPassive w = wrapMRPassive (treeModelFilterGetModel w)


-- @T: treeModelFilterSetVisibleFunc					 | ["tree","Model","Filter","Set","Visible","Func"]
treeModelFilterSetVisibleFuncReactive :: TreeModelFilterClass self => (self) -> ReactiveFieldWrite IO ((TreeIter -> IO Bool))
treeModelFilterSetVisibleFuncReactive w = wrapMW (treeModelFilterSetVisibleFunc w)


-- @S: rowChanged
-- TODO
-- @S: rowDeleted
-- TODO
-- @S: rowHasChildToggled
-- TODO
-- @S: rowInserted
-- TODO
-- @S: rowsReordered
-- TODO
-- @G: treeModelSortGetModel					 | ["tree","Model","Sort","Get","Model"]
treeModelSortGetModelPassive :: TreeModelSortClass self => (self) -> ReactiveFieldRead IO (TreeModel)
treeModelSortGetModelPassive w = wrapMRPassive (treeModelSortGetModel w)


-- @G: treeModelGetFlags					 | ["tree","Model","Get","Flags"]
treeModelGetFlagsPassive :: TreeModelClass self => (self) -> ReactiveFieldRead IO ([TreeModelFlags])
treeModelGetFlagsPassive w = wrapMRPassive (treeModelGetFlags w)


-- @G: treeModelGetIterFirst					 | ["tree","Model","Get","Iter","First"]
treeModelGetIterFirstPassive :: TreeModelClass self => (self) -> ReactiveFieldRead IO ((Maybe TreeIter))
treeModelGetIterFirstPassive w = wrapMRPassive (treeModelGetIterFirst w)


-- @G: treeModelGetIterFromString					 | ["tree","Model","Get","Iter","From","String"]
-- TODO
-- @G: treeModelGetIter					 | ["tree","Model","Get","Iter"]
-- TODO
-- @G: treeModelGetPath					 | ["tree","Model","Get","Path"]
-- TODO
-- @G: treeModelGetStringFromIter					 | ["tree","Model","Get","String","From","Iter"]
-- TODO
-- @G: treeModelGetValue					 | ["tree","Model","Get","Value"]
-- TODO
-- @G: treeRowReferenceGetPath					 | ["tree","Row","Reference","Get","Path"]
treeRowReferenceGetPathPassive :: (TreeRowReference) -> ReactiveFieldRead IO (TreePath)
treeRowReferenceGetPathPassive w = wrapMRPassive (treeRowReferenceGetPath w)


-- @C: afterSelectionChanged
afterSelectionChangedReactive :: TreeSelectionClass self => self -> ReactiveFieldRead IO ()
afterSelectionChangedReactive w = reactivePropertyH_ w afterSelectionChanged

-- @C: onSelectionChanged
onSelectionChangedReactive :: TreeSelectionClass self => self -> ReactiveFieldRead IO ()
onSelectionChangedReactive w = reactivePropertyH_ w onSelectionChanged

-- @G: treeSelectionGetMode					 | ["tree","Selection","Get","Mode"]
treeSelectionGetModePassive :: TreeSelectionClass self => (self) -> ReactiveFieldRead IO (SelectionMode)
treeSelectionGetModePassive w = wrapMRPassive (treeSelectionGetMode w)


-- @G: treeSelectionGetSelected					 | ["tree","Selection","Get","Selected"]
treeSelectionGetSelectedPassive :: TreeSelectionClass self => (self) -> ReactiveFieldRead IO ((Maybe TreeIter))
treeSelectionGetSelectedPassive w = wrapMRPassive (treeSelectionGetSelected w)


-- @G: treeSelectionGetSelectedRows					 | ["tree","Selection","Get","Selected","Rows"]
treeSelectionGetSelectedRowsPassive :: TreeSelectionClass self => (self) -> ReactiveFieldRead IO ([TreePath])
treeSelectionGetSelectedRowsPassive w = wrapMRPassive (treeSelectionGetSelectedRows w)


-- @G: treeSelectionGetTreeView					 | ["tree","Selection","Get","Tree","View"]
treeSelectionGetTreeViewPassive :: TreeSelectionClass self => (self) -> ReactiveFieldRead IO (TreeView)
treeSelectionGetTreeViewPassive w = wrapMRPassive (treeSelectionGetTreeView w)


-- @A: treeSelectionMode
treeSelectionModePassive :: TreeSelectionClass self => (self) -> ReactiveFieldReadWrite IO (SelectionMode)
treeSelectionModePassive w = passivePropertyNE w treeSelectionMode


-- @S: treeSelectionSelectionChanged
treeSelectionSelectionChangedReactive :: TreeSelectionClass self => self -> ReactiveFieldRead IO ()
treeSelectionSelectionChangedReactive = (`reactiveSignalIO` treeSelectionSelectionChanged)


-- @T: treeSelectionSetMode					 | ["tree","Selection","Set","Mode"]
treeSelectionSetModeReactive :: TreeSelectionClass self => (self) -> ReactiveFieldWrite IO (SelectionMode)
treeSelectionSetModeReactive w = wrapMW (treeSelectionSetMode w)


-- @T: treeSelectionSetSelectFunction					 | ["tree","Selection","Set","Select","Function"]
treeSelectionSetSelectFunctionReactive :: TreeSelectionClass self => (self) -> ReactiveFieldWrite IO (TreeSelectionCB)
treeSelectionSetSelectFunctionReactive w = wrapMW (treeSelectionSetSelectFunction w)


-- @S: sortColumnChanged
sortColumnChangedReactive :: TreeSortableClass self => self -> ReactiveFieldRead IO ()
sortColumnChangedReactive = (`reactiveSignalIO` sortColumnChanged)


-- @G: treeSortableGetSortColumnId					 | ["tree","Sortable","Get","Sort","Column","Id"]
treeSortableGetSortColumnIdPassive :: TreeSortableClass self => (self) -> ReactiveFieldRead IO ((SortType, Bool, SortColumnId))
treeSortableGetSortColumnIdPassive w = wrapMRPassive (treeSortableGetSortColumnId w)


-- @T: treeSortableSetDefaultSortFunc					 | ["tree","Sortable","Set","Default","Sort","Func"]
treeSortableSetDefaultSortFuncReactive :: TreeSortableClass self => (self) -> ReactiveFieldWrite IO (Maybe (TreeIter -> TreeIter -> IO Ordering))
treeSortableSetDefaultSortFuncReactive w = wrapMW (treeSortableSetDefaultSortFunc w)


-- @T: treeSortableSetSortColumnId					 | ["tree","Sortable","Set","Sort","Column","Id"]
-- TODO
-- @T: treeSortableSetSortFunc					 | ["tree","Sortable","Set","Sort","Func"]
-- TODO
-- @G: treeStoreGetTree					 | ["tree","Store","Get","Tree"]
-- TODO
-- @G: treeStoreGetValue					 | ["tree","Store","Get","Value"]
-- TODO
-- @T: treeStoreSetValue					 | ["tree","Store","Set","Value"]
-- TODO
-- @C: afterColumnsChanged
afterColumnsChangedReactive :: TreeViewClass self => self -> ReactiveFieldRead IO ()
afterColumnsChangedReactive w = reactivePropertyH_ w afterColumnsChanged

-- @C: afterCursorChanged
afterCursorChangedReactive :: TreeViewClass self => self -> ReactiveFieldRead IO ()
afterCursorChangedReactive w = reactivePropertyH_ w afterCursorChanged

-- @C: afterRowActivated
-- TODO
-- @C: afterRowCollapsed
-- TODO
-- @C: afterRowExpanded
-- TODO
-- @C: afterStartInteractiveSearch
afterStartInteractiveSearchReactive :: TreeViewClass self => self -> ReactiveFieldRead IO ()
afterStartInteractiveSearchReactive w = reactivePropertyH_ w afterStartInteractiveSearch

-- @C: afterTestCollapseRow
-- TODO
-- @C: afterTestExpandRow
-- TODO
-- @C: afterColClicked
afterColClickedReactive :: TreeViewColumnClass self => self -> ReactiveFieldRead IO ()
afterColClickedReactive w = reactivePropertyH_ w afterColClicked

-- @C: onColClicked
onColClickedReactive :: TreeViewColumnClass self => self -> ReactiveFieldRead IO ()
onColClickedReactive w = reactivePropertyH_ w onColClicked

-- @S: columnsChanged
columnsChangedReactive :: TreeViewClass self => self -> ReactiveFieldRead IO ()
columnsChangedReactive = (`reactiveSignalIO` columnsChanged)


-- @A: treeViewColumnAlignment
treeViewColumnAlignmentPassive :: TreeViewColumn -> ReactiveFieldReadWrite IO Float
treeViewColumnAlignmentPassive w = passivePropertyNE w treeViewColumnAlignment


-- @A: treeViewColumnClickable
treeViewColumnClickablePassive :: TreeViewColumn -> ReactiveFieldReadWrite IO Bool
treeViewColumnClickablePassive w = passivePropertyNE w treeViewColumnClickable


-- @A: treeViewColumnExpand
treeViewColumnExpandPassive :: TreeViewColumn -> ReactiveFieldReadWrite IO Bool
treeViewColumnExpandPassive w = passivePropertyNE w treeViewColumnExpand


-- @A: treeViewColumnFixedWidth
treeViewColumnFixedWidthPassive :: TreeViewColumn -> ReactiveFieldReadWrite IO Int
treeViewColumnFixedWidthPassive w = passivePropertyNE w treeViewColumnFixedWidth


-- @G: treeViewColumnGetAlignment					 | ["tree","View","Column","Get","Alignment"]
treeViewColumnGetAlignmentPassive :: (TreeViewColumn) -> ReactiveFieldRead IO (Float)
treeViewColumnGetAlignmentPassive w = wrapMRPassive (treeViewColumnGetAlignment w)


-- @G: treeViewColumnGetCellRenderers					 | ["tree","View","Column","Get","Cell","Renderers"]
treeViewColumnGetCellRenderersPassive :: (TreeViewColumn) -> ReactiveFieldRead IO ([CellRenderer])
treeViewColumnGetCellRenderersPassive w = wrapMRPassive (treeViewColumnGetCellRenderers w)


-- @G: treeViewColumnGetClickable					 | ["tree","View","Column","Get","Clickable"]
treeViewColumnGetClickablePassive :: (TreeViewColumn) -> ReactiveFieldRead IO (Bool)
treeViewColumnGetClickablePassive w = wrapMRPassive (treeViewColumnGetClickable w)


-- @G: treeViewColumnGetExpand					 | ["tree","View","Column","Get","Expand"]
treeViewColumnGetExpandPassive :: (TreeViewColumn) -> ReactiveFieldRead IO (Bool)
treeViewColumnGetExpandPassive w = wrapMRPassive (treeViewColumnGetExpand w)


-- @G: treeViewColumnGetFixedWidth					 | ["tree","View","Column","Get","Fixed","Width"]
treeViewColumnGetFixedWidthPassive :: (TreeViewColumn) -> ReactiveFieldRead IO (Int)
treeViewColumnGetFixedWidthPassive w = wrapMRPassive (treeViewColumnGetFixedWidth w)


-- @G: treeViewColumnGetMaxWidth					 | ["tree","View","Column","Get","Max","Width"]
treeViewColumnGetMaxWidthPassive :: (TreeViewColumn) -> ReactiveFieldRead IO (Int)
treeViewColumnGetMaxWidthPassive w = wrapMRPassive (treeViewColumnGetMaxWidth w)


-- @G: treeViewColumnGetMinWidth					 | ["tree","View","Column","Get","Min","Width"]
treeViewColumnGetMinWidthPassive :: (TreeViewColumn) -> ReactiveFieldRead IO (Int)
treeViewColumnGetMinWidthPassive w = wrapMRPassive (treeViewColumnGetMinWidth w)


-- @G: treeViewColumnGetReorderable					 | ["tree","View","Column","Get","Reorderable"]
treeViewColumnGetReorderablePassive :: (TreeViewColumn) -> ReactiveFieldRead IO (Bool)
treeViewColumnGetReorderablePassive w = wrapMRPassive (treeViewColumnGetReorderable w)


-- @G: treeViewColumnGetResizable					 | ["tree","View","Column","Get","Resizable"]
treeViewColumnGetResizablePassive :: (TreeViewColumn) -> ReactiveFieldRead IO (Bool)
treeViewColumnGetResizablePassive w = wrapMRPassive (treeViewColumnGetResizable w)


-- @G: treeViewColumnGetSizing					 | ["tree","View","Column","Get","Sizing"]
treeViewColumnGetSizingPassive :: (TreeViewColumn) -> ReactiveFieldRead IO (TreeViewColumnSizing)
treeViewColumnGetSizingPassive w = wrapMRPassive (treeViewColumnGetSizing w)


-- @G: treeViewColumnGetSortColumnId					 | ["tree","View","Column","Get","Sort","Column","Id"]
treeViewColumnGetSortColumnIdPassive :: (TreeViewColumn) -> ReactiveFieldRead IO (SortColumnId)
treeViewColumnGetSortColumnIdPassive w = wrapMRPassive (treeViewColumnGetSortColumnId w)


-- @G: treeViewColumnGetSortIndicator					 | ["tree","View","Column","Get","Sort","Indicator"]
treeViewColumnGetSortIndicatorPassive :: (TreeViewColumn) -> ReactiveFieldRead IO (Bool)
treeViewColumnGetSortIndicatorPassive w = wrapMRPassive (treeViewColumnGetSortIndicator w)


-- @G: treeViewColumnGetSortOrder					 | ["tree","View","Column","Get","Sort","Order"]
treeViewColumnGetSortOrderPassive :: (TreeViewColumn) -> ReactiveFieldRead IO (SortType)
treeViewColumnGetSortOrderPassive w = wrapMRPassive (treeViewColumnGetSortOrder w)


-- @G: treeViewColumnGetSpacing					 | ["tree","View","Column","Get","Spacing"]
treeViewColumnGetSpacingPassive :: (TreeViewColumn) -> ReactiveFieldRead IO (Int)
treeViewColumnGetSpacingPassive w = wrapMRPassive (treeViewColumnGetSpacing w)


-- @G: treeViewColumnGetTitle					 | ["tree","View","Column","Get","Title"]
treeViewColumnGetTitlePassive :: GlibString string => (TreeViewColumn) -> ReactiveFieldRead IO ((Maybe string))
treeViewColumnGetTitlePassive w = wrapMRPassive (treeViewColumnGetTitle w)


-- @G: treeViewColumnGetVisible					 | ["tree","View","Column","Get","Visible"]
treeViewColumnGetVisiblePassive :: (TreeViewColumn) -> ReactiveFieldRead IO (Bool)
treeViewColumnGetVisiblePassive w = wrapMRPassive (treeViewColumnGetVisible w)


-- @G: treeViewColumnGetWidget					 | ["tree","View","Column","Get","Widget"]
treeViewColumnGetWidgetPassive :: (TreeViewColumn) -> ReactiveFieldRead IO ((Maybe Widget))
treeViewColumnGetWidgetPassive w = wrapMRPassive (treeViewColumnGetWidget w)


-- @G: treeViewColumnGetWidth					 | ["tree","View","Column","Get","Width"]
treeViewColumnGetWidthPassive :: (TreeViewColumn) -> ReactiveFieldRead IO (Int)
treeViewColumnGetWidthPassive w = wrapMRPassive (treeViewColumnGetWidth w)


-- @A: treeViewColumnMaxWidth
treeViewColumnMaxWidthPassive :: TreeViewColumn -> ReactiveFieldReadWrite IO Int
treeViewColumnMaxWidthPassive w = passivePropertyNE w treeViewColumnMaxWidth


-- @A: treeViewColumnMinWidth
treeViewColumnMinWidthPassive :: TreeViewColumn -> ReactiveFieldReadWrite IO Int
treeViewColumnMinWidthPassive w = passivePropertyNE w treeViewColumnMinWidth


-- @A: treeViewColumnReorderable
treeViewColumnReorderablePassive :: TreeViewColumn -> ReactiveFieldReadWrite IO Bool
treeViewColumnReorderablePassive w = passivePropertyNE w treeViewColumnReorderable


-- @A: treeViewColumnResizable
treeViewColumnResizablePassive :: TreeViewColumn -> ReactiveFieldReadWrite IO Bool
treeViewColumnResizablePassive w = passivePropertyNE w treeViewColumnResizable


-- @T: treeViewColumnSetAlignment					 | ["tree","View","Column","Set","Alignment"]
treeViewColumnSetAlignmentPassive :: (TreeViewColumn) -> ReactiveFieldWrite IO (Float)
treeViewColumnSetAlignmentPassive w = wrapMW (treeViewColumnSetAlignment w)

-- @T: treeViewColumnSetClickable					 | ["tree","View","Column","Set","Clickable"]
treeViewColumnSetClickablePassive :: (TreeViewColumn) -> ReactiveFieldWrite IO (Bool)
treeViewColumnSetClickablePassive w = wrapMW (treeViewColumnSetClickable w)

-- @T: treeViewColumnSetExpand					 | ["tree","View","Column","Set","Expand"]
treeViewColumnSetExpandPassive :: (TreeViewColumn) -> ReactiveFieldWrite IO (Bool)
treeViewColumnSetExpandPassive w = wrapMW (treeViewColumnSetExpand w)

-- @T: treeViewColumnSetFixedWidth					 | ["tree","View","Column","Set","Fixed","Width"]
treeViewColumnSetFixedWidthPassive :: (TreeViewColumn) -> ReactiveFieldWrite IO (Int)
treeViewColumnSetFixedWidthPassive w = wrapMW (treeViewColumnSetFixedWidth w)

-- @T: treeViewColumnSetMaxWidth					 | ["tree","View","Column","Set","Max","Width"]
treeViewColumnSetMaxWidthPassive :: (TreeViewColumn) -> ReactiveFieldWrite IO (Int)
treeViewColumnSetMaxWidthPassive w = wrapMW (treeViewColumnSetMaxWidth w)

-- @T: treeViewColumnSetMinWidth					 | ["tree","View","Column","Set","Min","Width"]
treeViewColumnSetMinWidthPassive :: (TreeViewColumn) -> ReactiveFieldWrite IO (Int)
treeViewColumnSetMinWidthPassive w = wrapMW (treeViewColumnSetMinWidth w)

-- @T: treeViewColumnSetReorderable					 | ["tree","View","Column","Set","Reorderable"]
treeViewColumnSetReorderablePassive :: (TreeViewColumn) -> ReactiveFieldWrite IO (Bool)
treeViewColumnSetReorderablePassive w = wrapMW (treeViewColumnSetReorderable w)

-- @T: treeViewColumnSetResizable					 | ["tree","View","Column","Set","Resizable"]
treeViewColumnSetResizablePassive :: (TreeViewColumn) -> ReactiveFieldWrite IO (Bool)
treeViewColumnSetResizablePassive w = wrapMW (treeViewColumnSetResizable w)

-- @T: treeViewColumnSetSizing					 | ["tree","View","Column","Set","Sizing"]
treeViewColumnSetSizingPassive :: (TreeViewColumn) -> ReactiveFieldWrite IO (TreeViewColumnSizing)
treeViewColumnSetSizingPassive w = wrapMW (treeViewColumnSetSizing w)

-- @T: treeViewColumnSetSortColumnId					 | ["tree","View","Column","Set","Sort","Column","Id"]
treeViewColumnSetSortColumnIdPassive :: (TreeViewColumn) -> ReactiveFieldWrite IO (SortColumnId)
treeViewColumnSetSortColumnIdPassive w = wrapMW (treeViewColumnSetSortColumnId w)

-- @T: treeViewColumnSetSortIndicator					 | ["tree","View","Column","Set","Sort","Indicator"]
treeViewColumnSetSortIndicatorPassive :: (TreeViewColumn) -> ReactiveFieldWrite IO (Bool)
treeViewColumnSetSortIndicatorPassive w = wrapMW (treeViewColumnSetSortIndicator w)

-- @T: treeViewColumnSetSortOrder					 | ["tree","View","Column","Set","Sort","Order"]
treeViewColumnSetSortOrderPassive :: (TreeViewColumn) -> ReactiveFieldWrite IO (SortType)
treeViewColumnSetSortOrderPassive w = wrapMW (treeViewColumnSetSortOrder w)

-- @T: treeViewColumnSetSpacing					 | ["tree","View","Column","Set","Spacing"]
treeViewColumnSetSpacingPassive :: (TreeViewColumn) -> ReactiveFieldWrite IO (Int)
treeViewColumnSetSpacingPassive w = wrapMW (treeViewColumnSetSpacing w)

-- @T: treeViewColumnSetTitle					 | ["tree","View","Column","Set","Title"]
treeViewColumnSetTitleReactive :: GlibString string => (TreeViewColumn) -> ReactiveFieldWrite IO (string)
treeViewColumnSetTitleReactive w = wrapMW (treeViewColumnSetTitle w)


-- @T: treeViewColumnSetVisible					 | ["tree","View","Column","Set","Visible"]
treeViewColumnSetVisiblePassive :: (TreeViewColumn) -> ReactiveFieldWrite IO (Bool)
treeViewColumnSetVisiblePassive w = wrapMW (treeViewColumnSetVisible w)

-- @T: treeViewColumnSetWidget					 | ["tree","View","Column","Set","Widget"]
treeViewColumnSetWidgetReactive :: WidgetClass widget => (TreeViewColumn) -> ReactiveFieldWrite IO (Maybe widget)
treeViewColumnSetWidgetReactive w = wrapMW (treeViewColumnSetWidget w)


-- @A: treeViewColumnSizing
treeViewColumnSizingPassive :: TreeViewColumn -> ReactiveFieldReadWrite IO TreeViewColumnSizing
treeViewColumnSizingPassive w = passivePropertyNE w treeViewColumnSizing


-- @A: treeViewColumnSortColumnId
treeViewColumnSortColumnIdPassive :: TreeViewColumn -> ReactiveFieldReadWrite IO SortColumnId
treeViewColumnSortColumnIdPassive w = passivePropertyNE w treeViewColumnSortColumnId


-- @A: treeViewColumnSortIndicator
treeViewColumnSortIndicatorPassive :: TreeViewColumn -> ReactiveFieldReadWrite IO Bool
treeViewColumnSortIndicatorPassive w = passivePropertyNE w treeViewColumnSortIndicator


-- @A: treeViewColumnSortOrder
treeViewColumnSortOrderPassive :: TreeViewColumn -> ReactiveFieldReadWrite IO SortType
treeViewColumnSortOrderPassive w = passivePropertyNE w treeViewColumnSortOrder


-- @A: treeViewColumnSpacing
treeViewColumnSpacingPassive :: TreeViewColumn -> ReactiveFieldReadWrite IO Int
treeViewColumnSpacingPassive w = passivePropertyNE w treeViewColumnSpacing


-- @A: treeViewColumnVisible
treeViewColumnVisiblePassive :: TreeViewColumn -> ReactiveFieldReadWrite IO Bool
treeViewColumnVisiblePassive w = passivePropertyNE w treeViewColumnVisible


-- @S: cursorChanged
cursorChangedReactive :: TreeViewClass self => self -> ReactiveFieldRead IO ()
cursorChangedReactive = (`reactiveSignalIO` cursorChanged)


-- @C: onColumnsChanged
onColumnsChangedReactive :: TreeViewClass self => self -> ReactiveFieldRead IO ()
onColumnsChangedReactive w = reactivePropertyH_ w onColumnsChanged

-- @C: onCursorChanged
onCursorChangedReactive :: TreeViewClass self => self -> ReactiveFieldRead IO ()
onCursorChangedReactive w = reactivePropertyH_ w onCursorChanged

-- @C: onRowActivated
-- TODO
-- @C: onRowCollapsed
-- TODO
-- @C: onRowExpanded
-- TODO
-- @C: onStartInteractiveSearch
onStartInteractiveSearchReactive :: TreeViewClass self => self -> ReactiveFieldRead IO ()
onStartInteractiveSearchReactive w = reactivePropertyH_ w onStartInteractiveSearch

-- @C: onTestCollapseRow
-- TODO
-- @C: onTestExpandRow
-- TODO
-- @S: rowActivated
-- TODO
-- @S: rowCollapsed
-- TODO
-- @S: rowExpanded
-- TODO
-- @S: testCollapseRow
-- TODO
-- @S: testExpandRow
-- TODO
-- @A: treeViewEnableGridLines
treeViewEnableGridLinesPassive :: TreeViewClass self => (self) -> ReactiveFieldReadWrite IO (TreeViewGridLines)
treeViewEnableGridLinesPassive w = passivePropertyNE w treeViewEnableGridLines


-- @A: treeViewEnableSearch
treeViewEnableSearchPassive :: TreeViewClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
treeViewEnableSearchPassive w = passivePropertyNE w treeViewEnableSearch


-- @A: treeViewEnableTreeLines
treeViewEnableTreeLinesPassive :: TreeViewClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
treeViewEnableTreeLinesPassive w = passivePropertyNE w treeViewEnableTreeLines


-- @A: treeViewFixedHeightMode
treeViewFixedHeightModePassive :: TreeViewClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
treeViewFixedHeightModePassive w = passivePropertyNE w treeViewFixedHeightMode


-- @G: treeViewGetBackgroundArea					 | ["tree","View","Get","Background","Area"]
-- TODO
-- @G: treeViewGetCellArea					 | ["tree","View","Get","Cell","Area"]
-- TODO
-- @G: treeViewGetColumn					 | ["tree","View","Get","Column"]
-- TODO
-- @G: treeViewGetColumns					 | ["tree","View","Get","Columns"]
treeViewGetColumnsPassive :: TreeViewClass self => (self) -> ReactiveFieldRead IO ([TreeViewColumn])
treeViewGetColumnsPassive w = wrapMRPassive (treeViewGetColumns w)


-- @G: treeViewGetCursor					 | ["tree","View","Get","Cursor"]
treeViewGetCursorPassive :: TreeViewClass self => (self) -> ReactiveFieldRead IO ((TreePath, Maybe TreeViewColumn))
treeViewGetCursorPassive w = wrapMRPassive (treeViewGetCursor w)


-- @G: treeViewGetEnableSearch					 | ["tree","View","Get","Enable","Search"]
treeViewGetEnableSearchPassive :: TreeViewClass self => (self) -> ReactiveFieldRead IO (Bool)
treeViewGetEnableSearchPassive w = wrapMRPassive (treeViewGetEnableSearch w)


-- @G: treeViewGetEnableTreeLines					 | ["tree","View","Get","Enable","Tree","Lines"]
treeViewGetEnableTreeLinesPassive :: TreeViewClass self => (self) -> ReactiveFieldRead IO (Bool)
treeViewGetEnableTreeLinesPassive w = wrapMRPassive (treeViewGetEnableTreeLines w)


-- @G: treeViewGetExpanderColumn					 | ["tree","View","Get","Expander","Column"]
treeViewGetExpanderColumnPassive :: TreeViewClass self => (self) -> ReactiveFieldRead IO (TreeViewColumn)
treeViewGetExpanderColumnPassive w = wrapMRPassive (treeViewGetExpanderColumn w)


-- @G: treeViewGetFixedHeightMode					 | ["tree","View","Get","Fixed","Height","Mode"]
treeViewGetFixedHeightModePassive :: TreeViewClass self => (self) -> ReactiveFieldRead IO (Bool)
treeViewGetFixedHeightModePassive w = wrapMRPassive (treeViewGetFixedHeightMode w)


-- @G: treeViewGetGridLines					 | ["tree","View","Get","Grid","Lines"]
treeViewGetGridLinesPassive :: TreeViewClass self => (self) -> ReactiveFieldRead IO (TreeViewGridLines)
treeViewGetGridLinesPassive w = wrapMRPassive (treeViewGetGridLines w)


-- @G: treeViewGetHAdjustment					 | ["tree","View","Get","HAdjustment"]
treeViewGetHAdjustmentPassive :: TreeViewClass self => (self) -> ReactiveFieldRead IO ((Maybe Adjustment))
treeViewGetHAdjustmentPassive w = wrapMRPassive (treeViewGetHAdjustment w)


-- @G: treeViewGetHeadersClickable					 | ["tree","View","Get","Headers","Clickable"]
treeViewGetHeadersClickablePassive :: TreeViewClass self => (self) -> ReactiveFieldRead IO (Bool)
treeViewGetHeadersClickablePassive w = wrapMRPassive (treeViewGetHeadersClickable w)


-- @G: treeViewGetHeadersVisible					 | ["tree","View","Get","Headers","Visible"]
treeViewGetHeadersVisiblePassive :: TreeViewClass self => (self) -> ReactiveFieldRead IO (Bool)
treeViewGetHeadersVisiblePassive w = wrapMRPassive (treeViewGetHeadersVisible w)


-- @G: treeViewGetHoverExpand					 | ["tree","View","Get","Hover","Expand"]
treeViewGetHoverExpandPassive :: TreeViewClass self => (self) -> ReactiveFieldRead IO (Bool)
treeViewGetHoverExpandPassive w = wrapMRPassive (treeViewGetHoverExpand w)


-- @G: treeViewGetHoverSelection					 | ["tree","View","Get","Hover","Selection"]
treeViewGetHoverSelectionPassive :: TreeViewClass self => (self) -> ReactiveFieldRead IO (Bool)
treeViewGetHoverSelectionPassive w = wrapMRPassive (treeViewGetHoverSelection w)


-- @G: treeViewGetModel					 | ["tree","View","Get","Model"]
treeViewGetModelPassive :: TreeViewClass self => (self) -> ReactiveFieldRead IO ((Maybe TreeModel))
treeViewGetModelPassive w = wrapMRPassive (treeViewGetModel w)


-- @G: treeViewGetPathAtPos					 | ["tree","View","Get","Path","At","Pos"]
-- TODO
-- @G: treeViewGetReorderable					 | ["tree","View","Get","Reorderable"]
treeViewGetReorderablePassive :: TreeViewClass self => (self) -> ReactiveFieldRead IO (Bool)
treeViewGetReorderablePassive w = wrapMRPassive (treeViewGetReorderable w)


-- @G: treeViewGetRubberBanding					 | ["tree","View","Get","Rubber","Banding"]
treeViewGetRubberBandingPassive :: TreeViewClass self => (self) -> ReactiveFieldRead IO (Bool)
treeViewGetRubberBandingPassive w = wrapMRPassive (treeViewGetRubberBanding w)


-- @G: treeViewGetRulesHint					 | ["tree","View","Get","Rules","Hint"]
treeViewGetRulesHintPassive :: TreeViewClass self => (self) -> ReactiveFieldRead IO (Bool)
treeViewGetRulesHintPassive w = wrapMRPassive (treeViewGetRulesHint w)


-- @G: treeViewGetSearchColumn					 | ["tree","View","Get","Search","Column"]
treeViewGetSearchColumnPassive :: (TreeViewClass self, GlibString string) => (self) -> ReactiveFieldRead IO ((ColumnId row string))
treeViewGetSearchColumnPassive w = wrapMRPassive (treeViewGetSearchColumn w)


-- @G: treeViewGetSearchEntry					 | ["tree","View","Get","Search","Entry"]
treeViewGetSearchEntryPassive :: TreeViewClass self => (self) -> ReactiveFieldRead IO ((Maybe Entry))
treeViewGetSearchEntryPassive w = wrapMRPassive (treeViewGetSearchEntry w)


-- @G: treeViewGetSelection					 | ["tree","View","Get","Selection"]
treeViewGetSelectionPassive :: TreeViewClass self => (self) -> ReactiveFieldRead IO (TreeSelection)
treeViewGetSelectionPassive w = wrapMRPassive (treeViewGetSelection w)


-- @G: treeViewGetTooltipContext					 | ["tree","View","Get","Tooltip","Context"]
-- TODO
-- @G: treeViewGetVAdjustment					 | ["tree","View","Get","VAdjustment"]
treeViewGetVAdjustmentPassive :: TreeViewClass self => (self) -> ReactiveFieldRead IO ((Maybe Adjustment))
treeViewGetVAdjustmentPassive w = wrapMRPassive (treeViewGetVAdjustment w)


-- @G: treeViewGetVisibleRange					 | ["tree","View","Get","Visible","Range"]
treeViewGetVisibleRangePassive :: TreeViewClass self => (self) -> ReactiveFieldRead IO ((TreePath, TreePath))
treeViewGetVisibleRangePassive w = wrapMRPassive (treeViewGetVisibleRange w)


-- @G: treeViewGetVisibleRect					 | ["tree","View","Get","Visible","Rect"]
treeViewGetVisibleRectPassive :: TreeViewClass self => (self) -> ReactiveFieldRead IO (Rectangle)
treeViewGetVisibleRectPassive w = wrapMRPassive (treeViewGetVisibleRect w)


-- @A: treeViewGridLines
treeViewGridLinesPassive :: TreeViewClass self => (self) -> ReactiveFieldReadWrite IO (TreeViewGridLines)
treeViewGridLinesPassive w = passivePropertyNE w treeViewGridLines


-- @A: treeViewHAdjustment
treeViewHAdjustmentPassive :: TreeViewClass self => (self) -> ReactiveFieldReadWrite IO ((Maybe Adjustment))
treeViewHAdjustmentPassive w = passivePropertyNE w treeViewHAdjustment


-- @A: treeViewHeadersClickable
treeViewHeadersClickablePassive :: TreeViewClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
treeViewHeadersClickablePassive w = passivePropertyNE w treeViewHeadersClickable


-- @A: treeViewHeadersVisible
treeViewHeadersVisiblePassive :: TreeViewClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
treeViewHeadersVisiblePassive w = passivePropertyNE w treeViewHeadersVisible


-- @A: treeViewHoverExpand
treeViewHoverExpandPassive :: TreeViewClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
treeViewHoverExpandPassive w = passivePropertyNE w treeViewHoverExpand


-- @A: treeViewHoverSelection
treeViewHoverSelectionPassive :: TreeViewClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
treeViewHoverSelectionPassive w = passivePropertyNE w treeViewHoverSelection


-- @A: treeViewLevelIndentation
treeViewLevelIndentationPassive :: TreeViewClass self => (self) -> ReactiveFieldReadWrite IO (Int)
treeViewLevelIndentationPassive w = passivePropertyNE w treeViewLevelIndentation


-- @A: treeViewReorderable
treeViewReorderablePassive :: TreeViewClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
treeViewReorderablePassive w = passivePropertyNE w treeViewReorderable


-- @A: treeViewRubberBanding
treeViewRubberBandingPassive :: TreeViewClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
treeViewRubberBandingPassive w = passivePropertyNE w treeViewRubberBanding


-- @A: treeViewRulesHint
treeViewRulesHintPassive :: TreeViewClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
treeViewRulesHintPassive w = passivePropertyNE w treeViewRulesHint


-- @A: treeViewSearchColumn
treeViewSearchColumnPassive :: (TreeViewClass self, GlibString string) => (self) -> ReactiveFieldReadWrite IO ((ColumnId row string))
treeViewSearchColumnPassive w = passivePropertyNE w treeViewSearchColumn


-- @T: treeViewSetColumnDragFunction					 | ["tree","View","Set","Column","Drag","Function"]
treeViewSetColumnDragFunctionReactive :: TreeViewClass self => (self) -> ReactiveFieldWrite IO (Maybe
  (TreeViewColumn ->
     Maybe TreeViewColumn -> Maybe TreeViewColumn -> IO Bool))
treeViewSetColumnDragFunctionReactive w = wrapMW (treeViewSetColumnDragFunction w)


-- @T: treeViewSetCursor					 | ["tree","View","Set","Cursor"]
-- TODO
-- @T: treeViewSetCursorOnCell					 | ["tree","View","Set","Cursor","On","Cell"]
-- TODO
-- @T: treeViewSetEnableSearch					 | ["tree","View","Set","Enable","Search"]
treeViewSetEnableSearchReactive :: TreeViewClass self => (self) -> ReactiveFieldWrite IO (Bool)
treeViewSetEnableSearchReactive w = wrapMW (treeViewSetEnableSearch w)


-- @T: treeViewSetEnableTreeLines					 | ["tree","View","Set","Enable","Tree","Lines"]
treeViewSetEnableTreeLinesReactive :: TreeViewClass self => (self) -> ReactiveFieldWrite IO (Bool)
treeViewSetEnableTreeLinesReactive w = wrapMW (treeViewSetEnableTreeLines w)


-- @T: treeViewSetExpanderColumn					 | ["tree","View","Set","Expander","Column"]
treeViewSetExpanderColumnReactive :: TreeViewClass self => (self) -> ReactiveFieldWrite IO (Maybe TreeViewColumn)
treeViewSetExpanderColumnReactive w = wrapMW (treeViewSetExpanderColumn w)


-- @T: treeViewSetFixedHeightMode					 | ["tree","View","Set","Fixed","Height","Mode"]
treeViewSetFixedHeightModeReactive :: TreeViewClass self => (self) -> ReactiveFieldWrite IO (Bool)
treeViewSetFixedHeightModeReactive w = wrapMW (treeViewSetFixedHeightMode w)


-- @T: treeViewSetGridLines					 | ["tree","View","Set","Grid","Lines"]
treeViewSetGridLinesReactive :: TreeViewClass self => (self) -> ReactiveFieldWrite IO (TreeViewGridLines)
treeViewSetGridLinesReactive w = wrapMW (treeViewSetGridLines w)


-- @T: treeViewSetHAdjustment					 | ["tree","View","Set","HAdjustment"]
treeViewSetHAdjustmentReactive :: TreeViewClass self => (self) -> ReactiveFieldWrite IO (Maybe Adjustment)
treeViewSetHAdjustmentReactive w = wrapMW (treeViewSetHAdjustment w)


-- @T: treeViewSetHeadersClickable					 | ["tree","View","Set","Headers","Clickable"]
treeViewSetHeadersClickableReactive :: TreeViewClass self => (self) -> ReactiveFieldWrite IO (Bool)
treeViewSetHeadersClickableReactive w = wrapMW (treeViewSetHeadersClickable w)


-- @T: treeViewSetHeadersVisible					 | ["tree","View","Set","Headers","Visible"]
treeViewSetHeadersVisibleReactive :: TreeViewClass self => (self) -> ReactiveFieldWrite IO (Bool)
treeViewSetHeadersVisibleReactive w = wrapMW (treeViewSetHeadersVisible w)


-- @T: treeViewSetHoverExpand					 | ["tree","View","Set","Hover","Expand"]
treeViewSetHoverExpandReactive :: TreeViewClass self => (self) -> ReactiveFieldWrite IO (Bool)
treeViewSetHoverExpandReactive w = wrapMW (treeViewSetHoverExpand w)


-- @T: treeViewSetHoverSelection					 | ["tree","View","Set","Hover","Selection"]
treeViewSetHoverSelectionReactive :: TreeViewClass self => (self) -> ReactiveFieldWrite IO (Bool)
treeViewSetHoverSelectionReactive w = wrapMW (treeViewSetHoverSelection w)


-- @T: treeViewSetModel					 | ["tree","View","Set","Model"]
treeViewSetModelReactive :: (TreeViewClass self, TreeModelClass model) => (self) -> ReactiveFieldWrite IO (model)
treeViewSetModelReactive w = wrapMW (treeViewSetModel w)


-- @T: treeViewSetReorderable					 | ["tree","View","Set","Reorderable"]
treeViewSetReorderableReactive :: TreeViewClass self => (self) -> ReactiveFieldWrite IO (Bool)
treeViewSetReorderableReactive w = wrapMW (treeViewSetReorderable w)


-- @T: treeViewSetRowSeparatorFunc					 | ["tree","View","Set","Row","Separator","Func"]
treeViewSetRowSeparatorFuncReactive :: TreeViewClass self => (self) -> ReactiveFieldWrite IO (Maybe (TreeIter -> IO Bool))
treeViewSetRowSeparatorFuncReactive w = wrapMW (treeViewSetRowSeparatorFunc w)


-- @T: treeViewSetRubberBanding					 | ["tree","View","Set","Rubber","Banding"]
treeViewSetRubberBandingReactive :: TreeViewClass self => (self) -> ReactiveFieldWrite IO (Bool)
treeViewSetRubberBandingReactive w = wrapMW (treeViewSetRubberBanding w)


-- @T: treeViewSetRulesHint					 | ["tree","View","Set","Rules","Hint"]
treeViewSetRulesHintReactive :: TreeViewClass self => (self) -> ReactiveFieldWrite IO (Bool)
treeViewSetRulesHintReactive w = wrapMW (treeViewSetRulesHint w)


-- @T: treeViewSetSearchColumn					 | ["tree","View","Set","Search","Column"]
treeViewSetSearchColumnReactive :: (TreeViewClass self, GlibString string) => (self) -> ReactiveFieldWrite IO (ColumnId row string)
treeViewSetSearchColumnReactive w = wrapMW (treeViewSetSearchColumn w)


-- @T: treeViewSetSearchEntry					 | ["tree","View","Set","Search","Entry"]
treeViewSetSearchEntryReactive :: (TreeViewClass self, EntryClass entry) => (self) -> ReactiveFieldWrite IO (Maybe entry)
treeViewSetSearchEntryReactive w = wrapMW (treeViewSetSearchEntry w)


-- @T: treeViewSetSearchEqualFunc					 | ["tree","View","Set","Search","Equal","Func"]
treeViewSetSearchEqualFuncReactive :: (TreeViewClass self, GlibString string) => (self) -> ReactiveFieldWrite IO (Maybe (string -> TreeIter -> IO Bool))
treeViewSetSearchEqualFuncReactive w = wrapMW (treeViewSetSearchEqualFunc w)


-- @T: treeViewSetTooltipCell					 | ["tree","View","Set","Tooltip","Cell"]
-- TODO
-- @T: treeViewSetTooltipRow					 | ["tree","View","Set","Tooltip","Row"]
-- TODO
-- @T: treeViewSetVAdjustment					 | ["tree","View","Set","VAdjustment"]
treeViewSetVAdjustmentReactive :: TreeViewClass self => (self) -> ReactiveFieldWrite IO (Maybe Adjustment)
treeViewSetVAdjustmentReactive w = wrapMW (treeViewSetVAdjustment w)


-- @A: treeViewShowExpanders
treeViewShowExpandersPassive :: TreeViewClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
treeViewShowExpandersPassive w = passivePropertyNE w treeViewShowExpanders


-- @A: treeViewTooltipColumn
treeViewTooltipColumnPassive :: (TreeViewClass self, GlibString string) => (self) -> ReactiveFieldReadWrite IO ((ColumnId row string))
treeViewTooltipColumnPassive w = passivePropertyNE w treeViewTooltipColumn


-- @A: treeViewVAdjustment
treeViewVAdjustmentPassive :: TreeViewClass self => (self) -> ReactiveFieldReadWrite IO ((Maybe Adjustment))
treeViewVAdjustmentPassive w = passivePropertyNE w treeViewVAdjustment


-- @C: afterApplyTag
-- TODO
-- @C: afterBeginUserAction
afterBeginUserActionReactive :: TextBufferClass self => self -> ReactiveFieldRead IO ()
afterBeginUserActionReactive w = reactivePropertyH_ w afterBeginUserAction

-- @C: afterBufferChanged
afterBufferChangedReactive :: TextBufferClass self => self -> ReactiveFieldRead IO ()
afterBufferChangedReactive w = reactivePropertyH_ w afterBufferChanged

-- @C: afterBufferInsertText
-- TODO
-- @C: afterDeleteRange
-- TODO
-- @C: afterEndUserAction
afterEndUserActionReactive :: TextBufferClass self => self -> ReactiveFieldRead IO ()
afterEndUserActionReactive w = reactivePropertyH_ w afterEndUserAction

-- @C: afterInsertPixbuf
-- TODO
-- @C: afterMarkDeleted
-- TODO
-- @C: afterMarkSet
-- TODO
-- @C: afterModifiedChanged
afterModifiedChangedReactive :: TextBufferClass self => self -> ReactiveFieldRead IO ()
afterModifiedChangedReactive w = reactivePropertyH_ w afterModifiedChanged

-- @C: afterRemoveTag
-- TODO
-- @S: applyTag
-- TODO
-- @S: beginUserAction
beginUserActionReactive :: TextBufferClass self => self -> ReactiveFieldRead IO ()
beginUserActionReactive = (`reactiveSignalIO` beginUserAction)


-- @S: bufferChanged
bufferChangedReactive :: TextBufferClass self => self -> ReactiveFieldRead IO ()
bufferChangedReactive = (`reactiveSignalIO` bufferChanged)


-- @S: bufferInsertText
-- TODO
-- @S: deleteRange
-- TODO
-- @S: endUserAction
endUserActionReactive :: TextBufferClass self => self -> ReactiveFieldRead IO ()
endUserActionReactive = (`reactiveSignalIO` endUserAction)


-- @S: insertChildAnchor
-- TODO
-- @S: insertPixbuf
-- TODO
-- @S: markDeleted
-- TODO
-- @S: markSet
-- TODO
-- @S: modifiedChanged
modifiedChangedReactive :: TextBufferClass self => self -> ReactiveFieldRead IO ()
modifiedChangedReactive = (`reactiveSignalIO` modifiedChanged)


-- @C: onApplyTag
-- TODO
-- @C: onBeginUserAction
onBeginUserActionReactive :: TextBufferClass self => self -> ReactiveFieldRead IO ()
onBeginUserActionReactive w = reactivePropertyH_ w onBeginUserAction

-- @C: onBufferChanged
onBufferChangedReactive :: TextBufferClass self => self -> ReactiveFieldRead IO ()
onBufferChangedReactive w = reactivePropertyH_ w onBufferChanged

-- @C: onBufferInsertText
-- TODO
-- @C: onDeleteRange
-- TODO
-- @C: onEndUserAction
onEndUserActionReactive :: TextBufferClass self => self -> ReactiveFieldRead IO ()
onEndUserActionReactive w = reactivePropertyH_ w onEndUserAction

-- @C: onInsertPixbuf
-- TODO
-- @C: onMarkDeleted
-- TODO
-- @C: onMarkSet
-- TODO
-- @C: onModifiedChanged
onModifiedChangedReactive :: TextBufferClass self => self -> ReactiveFieldRead IO ()
onModifiedChangedReactive w = reactivePropertyH_ w onModifiedChanged

-- @C: onRemoveTag
-- TODO
-- @S: pasteDone
-- TODO
-- @S: removeTag
-- TODO
-- @G: textBufferGetBounds					 | ["text","Buffer","Get","Bounds"]
textBufferGetBoundsPassive :: TextBufferClass self => (self) -> ReactiveFieldRead IO ((TextIter, TextIter))
textBufferGetBoundsPassive w = wrapMRPassive (textBufferGetBounds w)


-- @G: textBufferGetByteString					 | ["text","Buffer","Get","Byte","String"]
-- TODO
-- @G: textBufferGetByteStringSlice					 | ["text","Buffer","Get","Byte","String","Slice"]
-- TODO
-- @G: textBufferGetCharCount					 | ["text","Buffer","Get","Char","Count"]
textBufferGetCharCountPassive :: TextBufferClass self => (self) -> ReactiveFieldRead IO (Int)
textBufferGetCharCountPassive w = wrapMRPassive (textBufferGetCharCount w)


-- @G: textBufferGetEndIter					 | ["text","Buffer","Get","End","Iter"]
textBufferGetEndIterPassive :: TextBufferClass self => (self) -> ReactiveFieldRead IO (TextIter)
textBufferGetEndIterPassive w = wrapMRPassive (textBufferGetEndIter w)


-- @G: textBufferGetInsert					 | ["text","Buffer","Get","Insert"]
textBufferGetInsertPassive :: TextBufferClass self => (self) -> ReactiveFieldRead IO (TextMark)
textBufferGetInsertPassive w = wrapMRPassive (textBufferGetInsert w)


-- @G: textBufferGetIterAtChildAnchor					 | ["text","Buffer","Get","Iter","At","Child","Anchor"]
-- TODO
-- @G: textBufferGetIterAtLine					 | ["text","Buffer","Get","Iter","At","Line"]
-- TODO
-- @G: textBufferGetIterAtLineOffset					 | ["text","Buffer","Get","Iter","At","Line","Offset"]
-- TODO
-- @G: textBufferGetIterAtMark					 | ["text","Buffer","Get","Iter","At","Mark"]
-- TODO
-- @G: textBufferGetIterAtOffset					 | ["text","Buffer","Get","Iter","At","Offset"]
-- TODO
-- @G: textBufferGetLineCount					 | ["text","Buffer","Get","Line","Count"]
textBufferGetLineCountPassive :: TextBufferClass self => (self) -> ReactiveFieldRead IO (Int)
textBufferGetLineCountPassive w = wrapMRPassive (textBufferGetLineCount w)


-- @G: textBufferGetMark					 | ["text","Buffer","Get","Mark"]
-- TODO
-- @G: textBufferGetModified					 | ["text","Buffer","Get","Modified"]
textBufferGetModifiedPassive :: TextBufferClass self => (self) -> ReactiveFieldRead IO (Bool)
textBufferGetModifiedPassive w = wrapMRPassive (textBufferGetModified w)


-- @G: textBufferGetSelectionBound					 | ["text","Buffer","Get","Selection","Bound"]
textBufferGetSelectionBoundPassive :: TextBufferClass self => (self) -> ReactiveFieldRead IO (TextMark)
textBufferGetSelectionBoundPassive w = wrapMRPassive (textBufferGetSelectionBound w)


-- @G: textBufferGetSelectionBounds					 | ["text","Buffer","Get","Selection","Bounds"]
textBufferGetSelectionBoundsPassive :: TextBufferClass self => (self) -> ReactiveFieldRead IO ((TextIter, TextIter))
textBufferGetSelectionBoundsPassive w = wrapMRPassive (textBufferGetSelectionBounds w)


-- @G: textBufferGetSlice					 | ["text","Buffer","Get","Slice"]
-- TODO
-- @G: textBufferGetStartIter					 | ["text","Buffer","Get","Start","Iter"]
textBufferGetStartIterPassive :: TextBufferClass self => (self) -> ReactiveFieldRead IO (TextIter)
textBufferGetStartIterPassive w = wrapMRPassive (textBufferGetStartIter w)


-- @G: textBufferGetTagTable					 | ["text","Buffer","Get","Tag","Table"]
textBufferGetTagTablePassive :: TextBufferClass self => (self) -> ReactiveFieldRead IO (TextTagTable)
textBufferGetTagTablePassive w = wrapMRPassive (textBufferGetTagTable w)


-- @G: textBufferGetText					 | ["text","Buffer","Get","Text"]
-- TODO
-- @A: textBufferModified
textBufferModifiedPassive :: TextBufferClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
textBufferModifiedPassive w = passivePropertyNE w textBufferModified


-- @T: textBufferSetByteString					 | ["text","Buffer","Set","Byte","String"]
textBufferSetByteStringReactive :: TextBufferClass self => (self) -> ReactiveFieldWrite IO (ByteString)
textBufferSetByteStringReactive w = wrapMW (textBufferSetByteString w)


-- @T: textBufferSetModified					 | ["text","Buffer","Set","Modified"]
textBufferSetModifiedReactive :: TextBufferClass self => (self) -> ReactiveFieldWrite IO (Bool)
textBufferSetModifiedReactive w = wrapMW (textBufferSetModified w)


-- @T: textBufferSetText					 | ["text","Buffer","Set","Text"]
textBufferSetTextReactive :: (TextBufferClass self, GlibString string) => (self) -> ReactiveFieldWrite IO (string)
textBufferSetTextReactive w = wrapMW (textBufferSetText w)


-- @A: textBufferText
textBufferTextPassive :: (TextBufferClass self, GlibString string) => (self) -> ReactiveFieldReadWrite IO (string)
textBufferTextPassive w = passivePropertyNE w textBufferText


-- @G: textIterGetAttributes					 | ["text","Iter","Get","Attributes"]
-- TODO
-- @G: textIterGetBuffer					 | ["text","Iter","Get","Buffer"]
textIterGetBufferPassive :: (TextIter) -> ReactiveFieldRead IO (TextBuffer)
textIterGetBufferPassive w = wrapMRPassive (textIterGetBuffer w)


-- @G: textIterGetChar					 | ["text","Iter","Get","Char"]
textIterGetCharPassive :: (TextIter) -> ReactiveFieldRead IO ((Maybe Char))
textIterGetCharPassive w = wrapMRPassive (textIterGetChar w)


-- @G: textIterGetCharsInLine					 | ["text","Iter","Get","Chars","In","Line"]
textIterGetCharsInLinePassive :: (TextIter) -> ReactiveFieldRead IO (Int)
textIterGetCharsInLinePassive w = wrapMRPassive (textIterGetCharsInLine w)


-- @G: textIterGetChildAnchor					 | ["text","Iter","Get","Child","Anchor"]
textIterGetChildAnchorPassive :: (TextIter) -> ReactiveFieldRead IO ((Maybe TextChildAnchor))
textIterGetChildAnchorPassive w = wrapMRPassive (textIterGetChildAnchor w)


-- @G: textIterGetLanguage					 | ["text","Iter","Get","Language"]
textIterGetLanguagePassive :: (TextIter) -> ReactiveFieldRead IO (Language)
textIterGetLanguagePassive w = wrapMRPassive (textIterGetLanguage w)


-- @G: textIterGetLine					 | ["text","Iter","Get","Line"]
textIterGetLinePassive :: (TextIter) -> ReactiveFieldRead IO (Int)
textIterGetLinePassive w = wrapMRPassive (textIterGetLine w)


-- @G: textIterGetLineOffset					 | ["text","Iter","Get","Line","Offset"]
textIterGetLineOffsetPassive :: (TextIter) -> ReactiveFieldRead IO (Int)
textIterGetLineOffsetPassive w = wrapMRPassive (textIterGetLineOffset w)


-- @G: textIterGetMarks					 | ["text","Iter","Get","Marks"]
textIterGetMarksPassive :: (TextIter) -> ReactiveFieldRead IO ([TextMark])
textIterGetMarksPassive w = wrapMRPassive (textIterGetMarks w)


-- @G: textIterGetOffset					 | ["text","Iter","Get","Offset"]
textIterGetOffsetPassive :: (TextIter) -> ReactiveFieldRead IO (Int)
textIterGetOffsetPassive w = wrapMRPassive (textIterGetOffset w)


-- @G: textIterGetPixbuf					 | ["text","Iter","Get","Pixbuf"]
textIterGetPixbufPassive :: (TextIter) -> ReactiveFieldRead IO ((Maybe Pixbuf))
textIterGetPixbufPassive w = wrapMRPassive (textIterGetPixbuf w)


-- @G: textIterGetSlice					 | ["text","Iter","Get","Slice"]
-- TODO
-- @G: textIterGetTags					 | ["text","Iter","Get","Tags"]
textIterGetTagsPassive :: (TextIter) -> ReactiveFieldRead IO ([TextTag])
textIterGetTagsPassive w = wrapMRPassive (textIterGetTags w)


-- @G: textIterGetText					 | ["text","Iter","Get","Text"]
-- TODO
-- @G: textIterGetToggledTags					 | ["text","Iter","Get","Toggled","Tags"]
-- TODO
-- @G: textIterGetVisibleLineOffset					 | ["text","Iter","Get","Visible","Line","Offset"]
textIterGetVisibleLineOffsetPassive :: (TextIter) -> ReactiveFieldRead IO (Int)
textIterGetVisibleLineOffsetPassive w = wrapMRPassive (textIterGetVisibleLineOffset w)


-- @G: textIterGetVisibleSlice					 | ["text","Iter","Get","Visible","Slice"]
-- TODO
-- @G: textIterGetVisibleText					 | ["text","Iter","Get","Visible","Text"]
-- TODO
-- @A: textIterLineOffset
textIterLineOffsetPassive :: TextIter -> ReactiveFieldReadWrite IO Int
textIterLineOffsetPassive w = passivePropertyNE w textIterLineOffset


-- @A: textIterLine
textIterLinePassive :: TextIter -> ReactiveFieldReadWrite IO Int
textIterLinePassive w = passivePropertyNE w textIterLine


-- @A: textIterOffset
textIterOffsetPassive :: TextIter -> ReactiveFieldReadWrite IO Int
textIterOffsetPassive w = passivePropertyNE w textIterOffset


-- @T: textIterSetLine					 | ["text","Iter","Set","Line"]
textIterSetLinePassive :: (TextIter) -> ReactiveFieldWrite IO (Int)
textIterSetLinePassive w = wrapMW (textIterSetLine w)

-- @T: textIterSetLineOffset					 | ["text","Iter","Set","Line","Offset"]
textIterSetLineOffsetPassive :: (TextIter) -> ReactiveFieldWrite IO (Int)
textIterSetLineOffsetPassive w = wrapMW (textIterSetLineOffset w)

-- @T: textIterSetOffset					 | ["text","Iter","Set","Offset"]
textIterSetOffsetPassive :: (TextIter) -> ReactiveFieldWrite IO (Int)
textIterSetOffsetPassive w = wrapMW (textIterSetOffset w)

-- @T: textIterSetVisibleLineOffset					 | ["text","Iter","Set","Visible","Line","Offset"]
textIterSetVisibleLineOffsetPassive :: (TextIter) -> ReactiveFieldWrite IO (Int)
textIterSetVisibleLineOffsetPassive w = wrapMW (textIterSetVisibleLineOffset w)

-- @A: textIterVisibleLineOffset
textIterVisibleLineOffsetPassive :: TextIter -> ReactiveFieldReadWrite IO Int
textIterVisibleLineOffsetPassive w = passivePropertyNE w textIterVisibleLineOffset


-- @G: textMarkGetBuffer					 | ["text","Mark","Get","Buffer"]
textMarkGetBufferPassive :: TextMarkClass self => (self) -> ReactiveFieldRead IO ((Maybe TextBuffer))
textMarkGetBufferPassive w = wrapMRPassive (textMarkGetBuffer w)


-- @G: textMarkGetDeleted					 | ["text","Mark","Get","Deleted"]
textMarkGetDeletedPassive :: TextMarkClass self => (self) -> ReactiveFieldRead IO (Bool)
textMarkGetDeletedPassive w = wrapMRPassive (textMarkGetDeleted w)


-- @G: textMarkGetLeftGravity					 | ["text","Mark","Get","Left","Gravity"]
textMarkGetLeftGravityPassive :: TextMarkClass self => (self) -> ReactiveFieldRead IO (Bool)
textMarkGetLeftGravityPassive w = wrapMRPassive (textMarkGetLeftGravity w)


-- @G: textMarkGetName					 | ["text","Mark","Get","Name"]
textMarkGetNamePassive :: TextMarkClass self => (self) -> ReactiveFieldRead IO ((Maybe MarkName))
textMarkGetNamePassive w = wrapMRPassive (textMarkGetName w)


-- @G: textMarkGetVisible					 | ["text","Mark","Get","Visible"]
textMarkGetVisiblePassive :: TextMarkClass self => (self) -> ReactiveFieldRead IO (Bool)
textMarkGetVisiblePassive w = wrapMRPassive (textMarkGetVisible w)


-- @T: textMarkSetVisible					 | ["text","Mark","Set","Visible"]
textMarkSetVisibleReactive :: TextMarkClass self => (self) -> ReactiveFieldWrite IO (Bool)
textMarkSetVisibleReactive w = wrapMW (textMarkSetVisible w)


-- @A: textMarkVisible
textMarkVisiblePassive :: TextMarkClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
textMarkVisiblePassive w = passivePropertyNE w textMarkVisible


-- @C: onTextTagEvent
-- TODO
-- @G: textTagTableGetSize					 | ["text","Tag","Table","Get","Size"]
textTagTableGetSizePassive :: TextTagTableClass self => (self) -> ReactiveFieldRead IO (Int)
textTagTableGetSizePassive w = wrapMRPassive (textTagTableGetSize w)


-- @A: textTagBackgroundFullHeight
textTagBackgroundFullHeightPassive :: TextTagClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
textTagBackgroundFullHeightPassive w = passivePropertyNE w textTagBackgroundFullHeight


-- @A: textTagBackgroundFullHeightSet
textTagBackgroundFullHeightSetPassive :: TextTagClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
textTagBackgroundFullHeightSetPassive w = passivePropertyNE w textTagBackgroundFullHeightSet


-- @A: textTagBackgroundGdk
textTagBackgroundGdkPassive :: TextTagClass self => (self) -> ReactiveFieldReadWrite IO (Color)
textTagBackgroundGdkPassive w = passivePropertyNE w textTagBackgroundGdk


-- @A: textTagBackgroundSet
textTagBackgroundSetPassive :: TextTagClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
textTagBackgroundSetPassive w = passivePropertyNE w textTagBackgroundSet


-- @A: textTagBackgroundStippleSet
textTagBackgroundStippleSetPassive :: TextTagClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
textTagBackgroundStippleSetPassive w = passivePropertyNE w textTagBackgroundStippleSet


-- @A: textTagDirection
textTagDirectionPassive :: TextTagClass self => (self) -> ReactiveFieldReadWrite IO (TextDirection)
textTagDirectionPassive w = passivePropertyNE w textTagDirection


-- @A: textTagEditable
textTagEditablePassive :: TextTagClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
textTagEditablePassive w = passivePropertyNE w textTagEditable


-- @A: textTagEditableSet
textTagEditableSetPassive :: TextTagClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
textTagEditableSetPassive w = passivePropertyNE w textTagEditableSet


-- @S: textTagEvent
-- TODO
-- @A: textTagFamily
textTagFamilyPassive :: (TextTagClass self, GlibString string) => (self) -> ReactiveFieldReadWrite IO (string)
textTagFamilyPassive w = passivePropertyNE w textTagFamily


-- @A: textTagFamilySet
textTagFamilySetPassive :: TextTagClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
textTagFamilySetPassive w = passivePropertyNE w textTagFamilySet


-- @A: textTagFontDesc
textTagFontDescPassive :: TextTagClass self => (self) -> ReactiveFieldReadWrite IO (FontDescription)
textTagFontDescPassive w = passivePropertyNE w textTagFontDesc


-- @A: textTagFont
textTagFontPassive :: (TextTagClass self, GlibString string) => (self) -> ReactiveFieldReadWrite IO (string)
textTagFontPassive w = passivePropertyNE w textTagFont


-- @A: textTagForegroundGdk
textTagForegroundGdkPassive :: TextTagClass self => (self) -> ReactiveFieldReadWrite IO (Color)
textTagForegroundGdkPassive w = passivePropertyNE w textTagForegroundGdk


-- @A: textTagForegroundSet
textTagForegroundSetPassive :: TextTagClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
textTagForegroundSetPassive w = passivePropertyNE w textTagForegroundSet


-- @A: textTagForegroundStippleSet
textTagForegroundStippleSetPassive :: TextTagClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
textTagForegroundStippleSetPassive w = passivePropertyNE w textTagForegroundStippleSet


-- @G: textTagGetPriority					 | ["text","Tag","Get","Priority"]
textTagGetPriorityPassive :: TextTagClass self => (self) -> ReactiveFieldRead IO (Int)
textTagGetPriorityPassive w = wrapMRPassive (textTagGetPriority w)


-- @A: textTagIndent
textTagIndentPassive :: TextTagClass self => (self) -> ReactiveFieldReadWrite IO (Int)
textTagIndentPassive w = passivePropertyNE w textTagIndent


-- @A: textTagIndentSet
textTagIndentSetPassive :: TextTagClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
textTagIndentSetPassive w = passivePropertyNE w textTagIndentSet


-- @A: textTagInvisible
textTagInvisiblePassive :: TextTagClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
textTagInvisiblePassive w = passivePropertyNE w textTagInvisible


-- @A: textTagInvisibleSet
textTagInvisibleSetPassive :: TextTagClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
textTagInvisibleSetPassive w = passivePropertyNE w textTagInvisibleSet


-- @A: textTagJustification
textTagJustificationPassive :: TextTagClass self => (self) -> ReactiveFieldReadWrite IO (Justification)
textTagJustificationPassive w = passivePropertyNE w textTagJustification


-- @A: textTagJustificationSet
textTagJustificationSetPassive :: TextTagClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
textTagJustificationSetPassive w = passivePropertyNE w textTagJustificationSet


-- @A: textTagLanguage
textTagLanguagePassive :: (TextTagClass self, GlibString string) => (self) -> ReactiveFieldReadWrite IO (string)
textTagLanguagePassive w = passivePropertyNE w textTagLanguage


-- @A: textTagLanguageSet
textTagLanguageSetPassive :: TextTagClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
textTagLanguageSetPassive w = passivePropertyNE w textTagLanguageSet


-- @A: textTagLeftMargin
textTagLeftMarginPassive :: TextTagClass self => (self) -> ReactiveFieldReadWrite IO (Int)
textTagLeftMarginPassive w = passivePropertyNE w textTagLeftMargin


-- @A: textTagLeftMarginSet
textTagLeftMarginSetPassive :: TextTagClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
textTagLeftMarginSetPassive w = passivePropertyNE w textTagLeftMarginSet


-- @A: textTagName
textTagNamePassive :: (TextTagClass self, GlibString string) => (self) -> ReactiveFieldReadWrite IO ((Maybe string))
textTagNamePassive w = passivePropertyNE w textTagName


-- @A: textTagParagraphBackgroundGdk
textTagParagraphBackgroundGdkPassive :: TextTagClass self => (self) -> ReactiveFieldReadWrite IO (Color)
textTagParagraphBackgroundGdkPassive w = passivePropertyNE w textTagParagraphBackgroundGdk


-- @A: textTagParagraphBackgroundSet
textTagParagraphBackgroundSetPassive :: TextTagClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
textTagParagraphBackgroundSetPassive w = passivePropertyNE w textTagParagraphBackgroundSet


-- @A: textTagPixelsAboveLines
textTagPixelsAboveLinesPassive :: TextTagClass self => (self) -> ReactiveFieldReadWrite IO (Int)
textTagPixelsAboveLinesPassive w = passivePropertyNE w textTagPixelsAboveLines


-- @A: textTagPixelsAboveLinesSet
textTagPixelsAboveLinesSetPassive :: TextTagClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
textTagPixelsAboveLinesSetPassive w = passivePropertyNE w textTagPixelsAboveLinesSet


-- @A: textTagPixelsBelowLines
textTagPixelsBelowLinesPassive :: TextTagClass self => (self) -> ReactiveFieldReadWrite IO (Int)
textTagPixelsBelowLinesPassive w = passivePropertyNE w textTagPixelsBelowLines


-- @A: textTagPixelsBelowLinesSet
textTagPixelsBelowLinesSetPassive :: TextTagClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
textTagPixelsBelowLinesSetPassive w = passivePropertyNE w textTagPixelsBelowLinesSet


-- @A: textTagPixelsInsideWrap
textTagPixelsInsideWrapPassive :: TextTagClass self => (self) -> ReactiveFieldReadWrite IO (Int)
textTagPixelsInsideWrapPassive w = passivePropertyNE w textTagPixelsInsideWrap


-- @A: textTagPixelsInsideWrapSet
textTagPixelsInsideWrapSetPassive :: TextTagClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
textTagPixelsInsideWrapSetPassive w = passivePropertyNE w textTagPixelsInsideWrapSet


-- @A: textTagPriority
textTagPriorityPassive :: TextTagClass self => (self) -> ReactiveFieldReadWrite IO (Int)
textTagPriorityPassive w = passivePropertyNE w textTagPriority


-- @A: textTagRightMargin
textTagRightMarginPassive :: TextTagClass self => (self) -> ReactiveFieldReadWrite IO (Int)
textTagRightMarginPassive w = passivePropertyNE w textTagRightMargin


-- @A: textTagRightMarginSet
textTagRightMarginSetPassive :: TextTagClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
textTagRightMarginSetPassive w = passivePropertyNE w textTagRightMarginSet


-- @A: textTagRise
textTagRisePassive :: TextTagClass self => (self) -> ReactiveFieldReadWrite IO (Int)
textTagRisePassive w = passivePropertyNE w textTagRise


-- @A: textTagRiseSet
textTagRiseSetPassive :: TextTagClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
textTagRiseSetPassive w = passivePropertyNE w textTagRiseSet


-- @A: textTagScale
textTagScalePassive :: TextTagClass self => (self) -> ReactiveFieldReadWrite IO (Double)
textTagScalePassive w = passivePropertyNE w textTagScale


-- @A: textTagScaleSet
textTagScaleSetPassive :: TextTagClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
textTagScaleSetPassive w = passivePropertyNE w textTagScaleSet


-- @T: textTagSetPriority					 | ["text","Tag","Set","Priority"]
textTagSetPriorityReactive :: TextTagClass self => (self) -> ReactiveFieldWrite IO (Int)
textTagSetPriorityReactive w = wrapMW (textTagSetPriority w)


-- @A: textTagSize
textTagSizePassive :: TextTagClass self => (self) -> ReactiveFieldReadWrite IO (Int)
textTagSizePassive w = passivePropertyNE w textTagSize


-- @A: textTagSizePoints
textTagSizePointsPassive :: TextTagClass self => (self) -> ReactiveFieldReadWrite IO (Double)
textTagSizePointsPassive w = passivePropertyNE w textTagSizePoints


-- @A: textTagSizeSet
textTagSizeSetPassive :: TextTagClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
textTagSizeSetPassive w = passivePropertyNE w textTagSizeSet


-- @A: textTagStretch
textTagStretchPassive :: TextTagClass self => (self) -> ReactiveFieldReadWrite IO (Stretch)
textTagStretchPassive w = passivePropertyNE w textTagStretch


-- @A: textTagStretchSet
textTagStretchSetPassive :: TextTagClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
textTagStretchSetPassive w = passivePropertyNE w textTagStretchSet


-- @A: textTagStrikethrough
textTagStrikethroughPassive :: TextTagClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
textTagStrikethroughPassive w = passivePropertyNE w textTagStrikethrough


-- @A: textTagStrikethroughSet
textTagStrikethroughSetPassive :: TextTagClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
textTagStrikethroughSetPassive w = passivePropertyNE w textTagStrikethroughSet


-- @A: textTagStyle
textTagStylePassive :: TextTagClass self => (self) -> ReactiveFieldReadWrite IO (FontStyle)
textTagStylePassive w = passivePropertyNE w textTagStyle


-- @A: textTagStyleSet
textTagStyleSetPassive :: TextTagClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
textTagStyleSetPassive w = passivePropertyNE w textTagStyleSet


-- @A: textTagTabsSet
textTagTabsSetPassive :: TextTagClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
textTagTabsSetPassive w = passivePropertyNE w textTagTabsSet


-- @A: textTagUnderline
textTagUnderlinePassive :: TextTagClass self => (self) -> ReactiveFieldReadWrite IO (Underline)
textTagUnderlinePassive w = passivePropertyNE w textTagUnderline


-- @A: textTagUnderlineSet
textTagUnderlineSetPassive :: TextTagClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
textTagUnderlineSetPassive w = passivePropertyNE w textTagUnderlineSet


-- @A: textTagVariant
textTagVariantPassive :: TextTagClass self => (self) -> ReactiveFieldReadWrite IO (Variant)
textTagVariantPassive w = passivePropertyNE w textTagVariant


-- @A: textTagVariantSet
textTagVariantSetPassive :: TextTagClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
textTagVariantSetPassive w = passivePropertyNE w textTagVariantSet


-- @A: textTagWeight
textTagWeightPassive :: TextTagClass self => (self) -> ReactiveFieldReadWrite IO (Int)
textTagWeightPassive w = passivePropertyNE w textTagWeight


-- @A: textTagWeightSet
textTagWeightSetPassive :: TextTagClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
textTagWeightSetPassive w = passivePropertyNE w textTagWeightSet


-- @A: textTagWrapMode
textTagWrapModePassive :: TextTagClass self => (self) -> ReactiveFieldReadWrite IO (WrapMode)
textTagWrapModePassive w = passivePropertyNE w textTagWrapMode


-- @A: textTagWrapModeSet
textTagWrapModeSetPassive :: TextTagClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
textTagWrapModeSetPassive w = passivePropertyNE w textTagWrapModeSet


-- @S: backspace
backspaceReactive :: TextViewClass self => self -> ReactiveFieldRead IO ()
backspaceReactive = (`reactiveSignalIO` backspace)


-- @S: copyClipboard
copyClipboardReactive :: TextViewClass self => self -> ReactiveFieldRead IO ()
copyClipboardReactive = (`reactiveSignalIO` copyClipboard)


-- @S: cutClipboard
cutClipboardReactive :: TextViewClass self => self -> ReactiveFieldRead IO ()
cutClipboardReactive = (`reactiveSignalIO` cutClipboard)


-- @S: deleteFromCursor
-- TODO
-- @S: insertAtCursor
-- TODO
-- @S: moveCursor
-- TODO
-- @S: moveFocus
-- TODO
-- @S: moveViewport
-- TODO
-- @S: pageHorizontally
-- TODO
-- @S: pasteClipboard
pasteClipboardReactive :: TextViewClass self => self -> ReactiveFieldRead IO ()
pasteClipboardReactive = (`reactiveSignalIO` pasteClipboard)


-- @S: populatePopup
-- TODO
-- @S: selectAll
-- TODO
-- @S: setAnchor
setAnchorReactive :: TextViewClass self => self -> ReactiveFieldRead IO ()
setAnchorReactive = (`reactiveSignalIO` setAnchor)


-- @S: setTextViewScrollAdjustments
-- TODO
-- @G: textChildAnchorGetDeleted					 | ["text","Child","Anchor","Get","Deleted"]
textChildAnchorGetDeletedPassive :: (TextChildAnchor) -> ReactiveFieldRead IO (Bool)
textChildAnchorGetDeletedPassive w = wrapMRPassive (textChildAnchorGetDeleted w)


-- @G: textChildAnchorGetWidgets					 | ["text","Child","Anchor","Get","Widgets"]
textChildAnchorGetWidgetsPassive :: (TextChildAnchor) -> ReactiveFieldRead IO ([Widget])
textChildAnchorGetWidgetsPassive w = wrapMRPassive (textChildAnchorGetWidgets w)


-- @A: textViewAcceptsTab
textViewAcceptsTabPassive :: TextViewClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
textViewAcceptsTabPassive w = passivePropertyNE w textViewAcceptsTab


-- @A: textViewBuffer
textViewBufferPassive :: TextViewClass self => (self) -> ReactiveFieldReadWrite IO (TextBuffer)
textViewBufferPassive w = passivePropertyNE w textViewBuffer


-- @A: textViewCursorVisible
textViewCursorVisiblePassive :: TextViewClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
textViewCursorVisiblePassive w = passivePropertyNE w textViewCursorVisible


-- @A: textViewEditable
textViewEditablePassive :: TextViewClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
textViewEditablePassive w = passivePropertyNE w textViewEditable


-- @G: textViewGetAcceptsTab					 | ["text","View","Get","Accepts","Tab"]
textViewGetAcceptsTabPassive :: TextViewClass self => (self) -> ReactiveFieldRead IO (Bool)
textViewGetAcceptsTabPassive w = wrapMRPassive (textViewGetAcceptsTab w)


-- @G: textViewGetBorderWindowSize					 | ["text","View","Get","Border","Window","Size"]
-- TODO
-- @G: textViewGetBuffer					 | ["text","View","Get","Buffer"]
textViewGetBufferPassive :: TextViewClass self => (self) -> ReactiveFieldRead IO (TextBuffer)
textViewGetBufferPassive w = wrapMRPassive (textViewGetBuffer w)


-- @G: textViewGetCursorVisible					 | ["text","View","Get","Cursor","Visible"]
textViewGetCursorVisiblePassive :: TextViewClass self => (self) -> ReactiveFieldRead IO (Bool)
textViewGetCursorVisiblePassive w = wrapMRPassive (textViewGetCursorVisible w)


-- @G: textViewGetDefaultAttributes					 | ["text","View","Get","Default","Attributes"]
textViewGetDefaultAttributesPassive :: TextViewClass self => (self) -> ReactiveFieldRead IO (TextAttributes)
textViewGetDefaultAttributesPassive w = wrapMRPassive (textViewGetDefaultAttributes w)


-- @G: textViewGetEditable					 | ["text","View","Get","Editable"]
textViewGetEditablePassive :: TextViewClass self => (self) -> ReactiveFieldRead IO (Bool)
textViewGetEditablePassive w = wrapMRPassive (textViewGetEditable w)


-- @G: textViewGetHadjustment					 | ["text","View","Get","Hadjustment"]
textViewGetHadjustmentPassive :: TextViewClass self => (self) -> ReactiveFieldRead IO (Adjustment)
textViewGetHadjustmentPassive w = wrapMRPassive (textViewGetHadjustment w)


-- @G: textViewGetIndent					 | ["text","View","Get","Indent"]
textViewGetIndentPassive :: TextViewClass self => (self) -> ReactiveFieldRead IO (Int)
textViewGetIndentPassive w = wrapMRPassive (textViewGetIndent w)


-- @G: textViewGetIterAtLocation					 | ["text","View","Get","Iter","At","Location"]
-- TODO
-- @G: textViewGetIterAtPosition					 | ["text","View","Get","Iter","At","Position"]
-- TODO
-- @G: textViewGetIterLocation					 | ["text","View","Get","Iter","Location"]
-- TODO
-- @G: textViewGetJustification					 | ["text","View","Get","Justification"]
textViewGetJustificationPassive :: TextViewClass self => (self) -> ReactiveFieldRead IO (Justification)
textViewGetJustificationPassive w = wrapMRPassive (textViewGetJustification w)


-- @G: textViewGetLeftMargin					 | ["text","View","Get","Left","Margin"]
textViewGetLeftMarginPassive :: TextViewClass self => (self) -> ReactiveFieldRead IO (Int)
textViewGetLeftMarginPassive w = wrapMRPassive (textViewGetLeftMargin w)


-- @G: textViewGetLineAtY					 | ["text","View","Get","Line","At","Y"]
-- TODO
-- @G: textViewGetLineYrange					 | ["text","View","Get","Line","Yrange"]
-- TODO
-- @G: textViewGetOverwrite					 | ["text","View","Get","Overwrite"]
textViewGetOverwritePassive :: TextViewClass self => (self) -> ReactiveFieldRead IO (Bool)
textViewGetOverwritePassive w = wrapMRPassive (textViewGetOverwrite w)


-- @G: textViewGetPixelsAboveLines					 | ["text","View","Get","Pixels","Above","Lines"]
textViewGetPixelsAboveLinesPassive :: TextViewClass self => (self) -> ReactiveFieldRead IO (Int)
textViewGetPixelsAboveLinesPassive w = wrapMRPassive (textViewGetPixelsAboveLines w)


-- @G: textViewGetPixelsBelowLines					 | ["text","View","Get","Pixels","Below","Lines"]
textViewGetPixelsBelowLinesPassive :: TextViewClass self => (self) -> ReactiveFieldRead IO (Int)
textViewGetPixelsBelowLinesPassive w = wrapMRPassive (textViewGetPixelsBelowLines w)


-- @G: textViewGetPixelsInsideWrap					 | ["text","View","Get","Pixels","Inside","Wrap"]
textViewGetPixelsInsideWrapPassive :: TextViewClass self => (self) -> ReactiveFieldRead IO (Int)
textViewGetPixelsInsideWrapPassive w = wrapMRPassive (textViewGetPixelsInsideWrap w)


-- @G: textViewGetRightMargin					 | ["text","View","Get","Right","Margin"]
textViewGetRightMarginPassive :: TextViewClass self => (self) -> ReactiveFieldRead IO (Int)
textViewGetRightMarginPassive w = wrapMRPassive (textViewGetRightMargin w)


-- @G: textViewGetVadjustment					 | ["text","View","Get","Vadjustment"]
textViewGetVadjustmentPassive :: TextViewClass self => (self) -> ReactiveFieldRead IO (Adjustment)
textViewGetVadjustmentPassive w = wrapMRPassive (textViewGetVadjustment w)


-- @G: textViewGetVisibleRect					 | ["text","View","Get","Visible","Rect"]
textViewGetVisibleRectPassive :: TextViewClass self => (self) -> ReactiveFieldRead IO (Rectangle)
textViewGetVisibleRectPassive w = wrapMRPassive (textViewGetVisibleRect w)


-- @G: textViewGetWindow					 | ["text","View","Get","Window"]
-- TODO
-- @G: textViewGetWindowType					 | ["text","View","Get","Window","Type"]
-- TODO
-- @G: textViewGetWrapMode					 | ["text","View","Get","Wrap","Mode"]
textViewGetWrapModePassive :: TextViewClass self => (self) -> ReactiveFieldRead IO (WrapMode)
textViewGetWrapModePassive w = wrapMRPassive (textViewGetWrapMode w)


-- @A: textViewImModule
textViewImModulePassive :: TextViewClass self => (self) -> ReactiveFieldReadWrite IO (DefaultGlibString)
textViewImModulePassive w = passivePropertyNE w textViewImModule


-- @A: textViewIndent
textViewIndentPassive :: TextViewClass self => (self) -> ReactiveFieldReadWrite IO (Int)
textViewIndentPassive w = passivePropertyNE w textViewIndent


-- @A: textViewJustification
textViewJustificationPassive :: TextViewClass self => (self) -> ReactiveFieldReadWrite IO (Justification)
textViewJustificationPassive w = passivePropertyNE w textViewJustification


-- @A: textViewLeftMargin
textViewLeftMarginPassive :: TextViewClass self => (self) -> ReactiveFieldReadWrite IO (Int)
textViewLeftMarginPassive w = passivePropertyNE w textViewLeftMargin


-- @A: textViewOverwrite
textViewOverwritePassive :: TextViewClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
textViewOverwritePassive w = passivePropertyNE w textViewOverwrite


-- @A: textViewPixelsAboveLines
textViewPixelsAboveLinesPassive :: TextViewClass self => (self) -> ReactiveFieldReadWrite IO (Int)
textViewPixelsAboveLinesPassive w = passivePropertyNE w textViewPixelsAboveLines


-- @A: textViewPixelsBelowLines
textViewPixelsBelowLinesPassive :: TextViewClass self => (self) -> ReactiveFieldReadWrite IO (Int)
textViewPixelsBelowLinesPassive w = passivePropertyNE w textViewPixelsBelowLines


-- @A: textViewPixelsInsideWrap
textViewPixelsInsideWrapPassive :: TextViewClass self => (self) -> ReactiveFieldReadWrite IO (Int)
textViewPixelsInsideWrapPassive w = passivePropertyNE w textViewPixelsInsideWrap


-- @S: textViewPreeditChanged
-- TODO
-- @A: textViewRightMargin
textViewRightMarginPassive :: TextViewClass self => (self) -> ReactiveFieldReadWrite IO (Int)
textViewRightMarginPassive w = passivePropertyNE w textViewRightMargin


-- @T: textViewSetAcceptsTab					 | ["text","View","Set","Accepts","Tab"]
textViewSetAcceptsTabReactive :: TextViewClass self => (self) -> ReactiveFieldWrite IO (Bool)
textViewSetAcceptsTabReactive w = wrapMW (textViewSetAcceptsTab w)


-- @T: textViewSetBorderWindowSize					 | ["text","View","Set","Border","Window","Size"]
-- TODO
-- @T: textViewSetBuffer					 | ["text","View","Set","Buffer"]
textViewSetBufferReactive :: (TextViewClass self, TextBufferClass buffer) => (self) -> ReactiveFieldWrite IO (buffer)
textViewSetBufferReactive w = wrapMW (textViewSetBuffer w)


-- @T: textViewSetCursorVisible					 | ["text","View","Set","Cursor","Visible"]
textViewSetCursorVisibleReactive :: TextViewClass self => (self) -> ReactiveFieldWrite IO (Bool)
textViewSetCursorVisibleReactive w = wrapMW (textViewSetCursorVisible w)


-- @T: textViewSetEditable					 | ["text","View","Set","Editable"]
textViewSetEditableReactive :: TextViewClass self => (self) -> ReactiveFieldWrite IO (Bool)
textViewSetEditableReactive w = wrapMW (textViewSetEditable w)


-- @T: textViewSetIndent					 | ["text","View","Set","Indent"]
textViewSetIndentReactive :: TextViewClass self => (self) -> ReactiveFieldWrite IO (Int)
textViewSetIndentReactive w = wrapMW (textViewSetIndent w)


-- @T: textViewSetJustification					 | ["text","View","Set","Justification"]
textViewSetJustificationReactive :: TextViewClass self => (self) -> ReactiveFieldWrite IO (Justification)
textViewSetJustificationReactive w = wrapMW (textViewSetJustification w)


-- @T: textViewSetLeftMargin					 | ["text","View","Set","Left","Margin"]
textViewSetLeftMarginReactive :: TextViewClass self => (self) -> ReactiveFieldWrite IO (Int)
textViewSetLeftMarginReactive w = wrapMW (textViewSetLeftMargin w)


-- @T: textViewSetOverwrite					 | ["text","View","Set","Overwrite"]
textViewSetOverwriteReactive :: TextViewClass self => (self) -> ReactiveFieldWrite IO (Bool)
textViewSetOverwriteReactive w = wrapMW (textViewSetOverwrite w)


-- @T: textViewSetPixelsAboveLines					 | ["text","View","Set","Pixels","Above","Lines"]
textViewSetPixelsAboveLinesReactive :: TextViewClass self => (self) -> ReactiveFieldWrite IO (Int)
textViewSetPixelsAboveLinesReactive w = wrapMW (textViewSetPixelsAboveLines w)


-- @T: textViewSetPixelsBelowLines					 | ["text","View","Set","Pixels","Below","Lines"]
textViewSetPixelsBelowLinesReactive :: TextViewClass self => (self) -> ReactiveFieldWrite IO (Int)
textViewSetPixelsBelowLinesReactive w = wrapMW (textViewSetPixelsBelowLines w)


-- @T: textViewSetPixelsInsideWrap					 | ["text","View","Set","Pixels","Inside","Wrap"]
textViewSetPixelsInsideWrapReactive :: TextViewClass self => (self) -> ReactiveFieldWrite IO (Int)
textViewSetPixelsInsideWrapReactive w = wrapMW (textViewSetPixelsInsideWrap w)


-- @T: textViewSetRightMargin					 | ["text","View","Set","Right","Margin"]
textViewSetRightMarginReactive :: TextViewClass self => (self) -> ReactiveFieldWrite IO (Int)
textViewSetRightMarginReactive w = wrapMW (textViewSetRightMargin w)


-- @T: textViewSetWrapMode					 | ["text","View","Set","Wrap","Mode"]
textViewSetWrapModeReactive :: TextViewClass self => (self) -> ReactiveFieldWrite IO (WrapMode)
textViewSetWrapModeReactive w = wrapMW (textViewSetWrapMode w)


-- @A: textViewWrapMode
textViewWrapModePassive :: TextViewClass self => (self) -> ReactiveFieldReadWrite IO (WrapMode)
textViewWrapModePassive w = passivePropertyNE w textViewWrapMode


-- @S: toggleCursorVisible
toggleCursorVisibleReactive :: TextViewClass self => self -> ReactiveFieldRead IO ()
toggleCursorVisibleReactive = (`reactiveSignalIO` toggleCursorVisible)


-- @S: toggleOverwrite
toggleOverwriteReactive :: TextViewClass self => self -> ReactiveFieldRead IO ()
toggleOverwriteReactive = (`reactiveSignalIO` toggleOverwrite)


-- @G: frameGetLabelAlign					 | ["frame","Get","Label","Align"]
frameGetLabelAlignPassive :: FrameClass self => (self) -> ReactiveFieldRead IO ((Float, Float))
frameGetLabelAlignPassive w = wrapMRPassive (frameGetLabelAlign w)


-- @G: frameGetLabel					 | ["frame","Get","Label"]
frameGetLabelPassive :: (FrameClass self, GlibString string) => (self) -> ReactiveFieldRead IO (string)
frameGetLabelPassive w = wrapMRPassive (frameGetLabel w)


-- @G: frameGetLabelWidget					 | ["frame","Get","Label","Widget"]
frameGetLabelWidgetPassive :: FrameClass self => (self) -> ReactiveFieldRead IO ((Maybe Widget))
frameGetLabelWidgetPassive w = wrapMRPassive (frameGetLabelWidget w)


-- @G: frameGetShadowType					 | ["frame","Get","Shadow","Type"]
frameGetShadowTypePassive :: FrameClass self => (self) -> ReactiveFieldRead IO (ShadowType)
frameGetShadowTypePassive w = wrapMRPassive (frameGetShadowType w)


-- @A: frameLabel
frameLabelPassive :: (FrameClass self, GlibString string) => (self) -> ReactiveFieldReadWrite IO (string)
frameLabelPassive w = passivePropertyNE w frameLabel


-- @A: frameLabelXAlign
frameLabelXAlignPassive :: FrameClass self => (self) -> ReactiveFieldReadWrite IO (Float)
frameLabelXAlignPassive w = passivePropertyNE w frameLabelXAlign


-- @A: frameLabelYAlign
frameLabelYAlignPassive :: FrameClass self => (self) -> ReactiveFieldReadWrite IO (Float)
frameLabelYAlignPassive w = passivePropertyNE w frameLabelYAlign


-- @T: frameSetLabelAlign					 | ["frame","Set","Label","Align"]
-- TODO
-- @T: frameSetLabel					 | ["frame","Set","Label"]
frameSetLabelReactive :: (FrameClass self, GlibString string) => (self) -> ReactiveFieldWrite IO (string)
frameSetLabelReactive w = wrapMW (frameSetLabel w)


-- @T: frameSetLabelWidget					 | ["frame","Set","Label","Widget"]
frameSetLabelWidgetReactive :: (FrameClass self, WidgetClass labelWidget) => (self) -> ReactiveFieldWrite IO (labelWidget)
frameSetLabelWidgetReactive w = wrapMW (frameSetLabelWidget w)


-- @T: frameSetShadowType					 | ["frame","Set","Shadow","Type"]
frameSetShadowTypeReactive :: FrameClass self => (self) -> ReactiveFieldWrite IO (ShadowType)
frameSetShadowTypeReactive w = wrapMW (frameSetShadowType w)


-- @A: frameShadowType
frameShadowTypePassive :: FrameClass self => (self) -> ReactiveFieldReadWrite IO (ShadowType)
frameShadowTypePassive w = passivePropertyNE w frameShadowType


-- @G: pageSetupGetBottomMargin					 | ["page","Setup","Get","Bottom","Margin"]
-- TODO
-- @G: pageSetupGetLeftMargin					 | ["page","Setup","Get","Left","Margin"]
-- TODO
-- @G: pageSetupGetPageHeight					 | ["page","Setup","Get","Page","Height"]
-- TODO
-- @G: pageSetupGetPageWidth					 | ["page","Setup","Get","Page","Width"]
-- TODO
-- @G: pageSetupGetPaperHeight					 | ["page","Setup","Get","Paper","Height"]
-- TODO
-- @G: pageSetupGetPaperWidth					 | ["page","Setup","Get","Paper","Width"]
-- TODO
-- @G: pageSetupGetRightMargin					 | ["page","Setup","Get","Right","Margin"]
-- TODO
-- @G: pageSetupGetTopMargin					 | ["page","Setup","Get","Top","Margin"]
-- TODO
-- @A: pageSetupOrientation
pageSetupOrientationPassive :: PageSetupClass self => (self) -> ReactiveFieldReadWrite IO (PageOrientation)
pageSetupOrientationPassive w = passivePropertyNE w pageSetupOrientation


-- @A: pageSetupPaperSize
pageSetupPaperSizePassive :: PageSetupClass self => (self) -> ReactiveFieldReadWrite IO (PaperSize)
pageSetupPaperSizePassive w = passivePropertyNE w pageSetupPaperSize


-- @T: pageSetupSetBottomMargin					 | ["page","Setup","Set","Bottom","Margin"]
-- TODO
-- @T: pageSetupSetLeftMargin					 | ["page","Setup","Set","Left","Margin"]
-- TODO
-- @T: pageSetupSetPaperSizeAndDefaultMargins					 | ["page","Setup","Set","Paper","Size","And","Default","Margins"]
pageSetupSetPaperSizeAndDefaultMarginsReactive :: PageSetupClass self => (self) -> ReactiveFieldWrite IO (PaperSize)
pageSetupSetPaperSizeAndDefaultMarginsReactive w = wrapMW (pageSetupSetPaperSizeAndDefaultMargins w)


-- @T: pageSetupSetRightMargin					 | ["page","Setup","Set","Right","Margin"]
-- TODO
-- @T: pageSetupSetTopMargin					 | ["page","Setup","Set","Top","Margin"]
-- TODO
-- @G: paperSizeGetDefaultBottomMargin					 | ["paper","Size","Get","Default","Bottom","Margin"]
-- TODO
-- @G: paperSizeGetDefaultLeftMargin					 | ["paper","Size","Get","Default","Left","Margin"]
-- TODO
-- @G: paperSizeGetDefaultRightMargin					 | ["paper","Size","Get","Default","Right","Margin"]
-- TODO
-- @G: paperSizeGetDefault					 | ["paper","Size","Get","Default"]
-- TODO
-- @G: paperSizeGetDefaultTopMargin					 | ["paper","Size","Get","Default","Top","Margin"]
-- TODO
-- @G: paperSizeGetDisplayName					 | ["paper","Size","Get","Display","Name"]
paperSizeGetDisplayNamePassive :: GlibString string => (PaperSize) -> ReactiveFieldRead IO (string)
paperSizeGetDisplayNamePassive w = wrapMRPassive (paperSizeGetDisplayName w)


-- @G: paperSizeGetHeight					 | ["paper","Size","Get","Height"]
-- TODO
-- @G: paperSizeGetName					 | ["paper","Size","Get","Name"]
paperSizeGetNamePassive :: GlibString string => (PaperSize) -> ReactiveFieldRead IO (string)
paperSizeGetNamePassive w = wrapMRPassive (paperSizeGetName w)


-- @G: paperSizeGetPaperSizes					 | ["paper","Size","Get","Paper","Sizes"]
paperSizeGetPaperSizesPassive :: (Bool) -> ReactiveFieldRead IO ([PaperSize])
paperSizeGetPaperSizesPassive w = wrapMRPassive (paperSizeGetPaperSizes w)


-- @G: paperSizeGetPpdName					 | ["paper","Size","Get","Ppd","Name"]
paperSizeGetPpdNamePassive :: GlibString string => (PaperSize) -> ReactiveFieldRead IO ((Maybe string))
paperSizeGetPpdNamePassive w = wrapMRPassive (paperSizeGetPpdName w)


-- @G: paperSizeGetWidth					 | ["paper","Size","Get","Width"]
-- TODO
-- @T: paperSizeSetSize					 | ["paper","Size","Set","Size"]
-- TODO
-- @G: printContextGetCairoContext					 | ["print","Context","Get","Cairo","Context"]
printContextGetCairoContextPassive :: PrintContextClass self => (self) -> ReactiveFieldRead IO (Cairo)
printContextGetCairoContextPassive w = wrapMRPassive (printContextGetCairoContext w)


-- @G: printContextGetDpiX					 | ["print","Context","Get","Dpi","X"]
printContextGetDpiXPassive :: PrintContextClass self => (self) -> ReactiveFieldRead IO (Double)
printContextGetDpiXPassive w = wrapMRPassive (printContextGetDpiX w)


-- @G: printContextGetDpiY					 | ["print","Context","Get","Dpi","Y"]
printContextGetDpiYPassive :: PrintContextClass self => (self) -> ReactiveFieldRead IO (Double)
printContextGetDpiYPassive w = wrapMRPassive (printContextGetDpiY w)


-- @G: printContextGetHardMargins					 | ["print","Context","Get","Hard","Margins"]
printContextGetHardMarginsPassive :: PrintContextClass self => (self) -> ReactiveFieldRead IO ((Maybe (Double, Double, Double, Double)))
printContextGetHardMarginsPassive w = wrapMRPassive (printContextGetHardMargins w)


-- @G: printContextGetHeight					 | ["print","Context","Get","Height"]
printContextGetHeightPassive :: PrintContextClass self => (self) -> ReactiveFieldRead IO (Double)
printContextGetHeightPassive w = wrapMRPassive (printContextGetHeight w)


-- @G: printContextGetPageSetup					 | ["print","Context","Get","Page","Setup"]
printContextGetPageSetupPassive :: PrintContextClass self => (self) -> ReactiveFieldRead IO (PageSetup)
printContextGetPageSetupPassive w = wrapMRPassive (printContextGetPageSetup w)


-- @G: printContextGetPangoFontmap					 | ["print","Context","Get","Pango","Fontmap"]
printContextGetPangoFontmapPassive :: PrintContextClass self => (self) -> ReactiveFieldRead IO (FontMap)
printContextGetPangoFontmapPassive w = wrapMRPassive (printContextGetPangoFontmap w)


-- @G: printContextGetWidth					 | ["print","Context","Get","Width"]
printContextGetWidthPassive :: PrintContextClass self => (self) -> ReactiveFieldRead IO (Double)
printContextGetWidthPassive w = wrapMRPassive (printContextGetWidth w)


-- @T: printContextSetCairoContext					 | ["print","Context","Set","Cairo","Context"]
-- TODO
-- @A: printOperationAllowAsync
printOperationAllowAsyncPassive :: PrintOperationClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
printOperationAllowAsyncPassive w = passivePropertyNE w printOperationAllowAsync


-- @A: printOperationCurrentPage
printOperationCurrentPagePassive :: PrintOperationClass self => (self) -> ReactiveFieldReadWrite IO (Int)
printOperationCurrentPagePassive w = passivePropertyNE w printOperationCurrentPage


-- @A: printOperationCustomTabLabel
printOperationCustomTabLabelPassive :: (PrintOperationClass self, GlibString string) => (self) -> ReactiveFieldReadWrite IO (string)
printOperationCustomTabLabelPassive w = passivePropertyNE w printOperationCustomTabLabel


-- @A: printOperationEmbedPageSetup
printOperationEmbedPageSetupPassive :: PrintOperationClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
printOperationEmbedPageSetupPassive w = passivePropertyNE w printOperationEmbedPageSetup


-- @A: printOperationExportFilename
printOperationExportFilenamePassive :: (PrintOperationClass self, GlibString string) => (self) -> ReactiveFieldReadWrite IO (string)
printOperationExportFilenamePassive w = passivePropertyNE w printOperationExportFilename


-- @G: printOperationGetError					 | ["print","Operation","Get","Error"]
printOperationGetErrorPassive :: PrintOperationClass self => (self) -> ReactiveFieldRead IO (())
printOperationGetErrorPassive w = wrapMRPassive (printOperationGetError w)


-- @G: printOperationGetNPagesToPrint					 | ["print","Operation","Get","NPages","To","Print"]
printOperationGetNPagesToPrintPassive :: PrintOperationClass self => (self) -> ReactiveFieldRead IO (Int)
printOperationGetNPagesToPrintPassive w = wrapMRPassive (printOperationGetNPagesToPrint w)


-- @G: printOperationGetStatus					 | ["print","Operation","Get","Status"]
printOperationGetStatusPassive :: PrintOperationClass self => (self) -> ReactiveFieldRead IO (PrintStatus)
printOperationGetStatusPassive w = wrapMRPassive (printOperationGetStatus w)


-- @G: printOperationGetStatusString					 | ["print","Operation","Get","Status","String"]
printOperationGetStatusStringPassive :: (PrintOperationClass self, GlibString string) => (self) -> ReactiveFieldRead IO (string)
printOperationGetStatusStringPassive w = wrapMRPassive (printOperationGetStatusString w)


-- @A: printOperationHasSelection
printOperationHasSelectionPassive :: PrintOperationClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
printOperationHasSelectionPassive w = passivePropertyNE w printOperationHasSelection


-- @A: printOperationJobName
printOperationJobNamePassive :: (PrintOperationClass self, GlibString string) => (self) -> ReactiveFieldReadWrite IO (string)
printOperationJobNamePassive w = passivePropertyNE w printOperationJobName


-- @A: printOperationNPages
printOperationNPagesPassive :: PrintOperationClass self => (self) -> ReactiveFieldReadWrite IO (Int)
printOperationNPagesPassive w = passivePropertyNE w printOperationNPages


-- @T: printOperationSetAllowAsync					 | ["print","Operation","Set","Allow","Async"]
printOperationSetAllowAsyncReactive :: PrintOperationClass self => (self) -> ReactiveFieldWrite IO (Bool)
printOperationSetAllowAsyncReactive w = wrapMW (printOperationSetAllowAsync w)


-- @T: printOperationSetCurrentPage					 | ["print","Operation","Set","Current","Page"]
printOperationSetCurrentPageReactive :: PrintOperationClass self => (self) -> ReactiveFieldWrite IO (Int)
printOperationSetCurrentPageReactive w = wrapMW (printOperationSetCurrentPage w)


-- @T: printOperationSetCustomTabLabel					 | ["print","Operation","Set","Custom","Tab","Label"]
printOperationSetCustomTabLabelReactive :: (PrintOperationClass self, GlibString string) => (self) -> ReactiveFieldWrite IO (string)
printOperationSetCustomTabLabelReactive w = wrapMW (printOperationSetCustomTabLabel w)


-- @T: printOperationSetDeferDrawing					 | ["print","Operation","Set","Defer","Drawing"]
-- TODO
-- @T: printOperationSetExportFilename					 | ["print","Operation","Set","Export","Filename"]
printOperationSetExportFilenameReactive :: (PrintOperationClass self, GlibString string) => (self) -> ReactiveFieldWrite IO (string)
printOperationSetExportFilenameReactive w = wrapMW (printOperationSetExportFilename w)


-- @T: printOperationSetJobName					 | ["print","Operation","Set","Job","Name"]
printOperationSetJobNameReactive :: (PrintOperationClass self, GlibString string) => (self) -> ReactiveFieldWrite IO (string)
printOperationSetJobNameReactive w = wrapMW (printOperationSetJobName w)


-- @T: printOperationSetNPages					 | ["print","Operation","Set","NPages"]
printOperationSetNPagesReactive :: PrintOperationClass self => (self) -> ReactiveFieldWrite IO (Int)
printOperationSetNPagesReactive w = wrapMW (printOperationSetNPages w)


-- @T: printOperationSetShowProgress					 | ["print","Operation","Set","Show","Progress"]
printOperationSetShowProgressReactive :: PrintOperationClass self => (self) -> ReactiveFieldWrite IO (Bool)
printOperationSetShowProgressReactive w = wrapMW (printOperationSetShowProgress w)


-- @T: printOperationSetTrackPrintStatus					 | ["print","Operation","Set","Track","Print","Status"]
printOperationSetTrackPrintStatusReactive :: PrintOperationClass self => (self) -> ReactiveFieldWrite IO (Bool)
printOperationSetTrackPrintStatusReactive w = wrapMW (printOperationSetTrackPrintStatus w)


-- @T: printOperationSetUnit					 | ["print","Operation","Set","Unit"]
printOperationSetUnitReactive :: PrintOperationClass self => (self) -> ReactiveFieldWrite IO (Unit)
printOperationSetUnitReactive w = wrapMW (printOperationSetUnit w)


-- @T: printOperationSetUseFullPage					 | ["print","Operation","Set","Use","Full","Page"]
printOperationSetUseFullPageReactive :: PrintOperationClass self => (self) -> ReactiveFieldWrite IO (Bool)
printOperationSetUseFullPageReactive w = wrapMW (printOperationSetUseFullPage w)


-- @A: printOperationShowProgress
printOperationShowProgressPassive :: PrintOperationClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
printOperationShowProgressPassive w = passivePropertyNE w printOperationShowProgress


-- @A: printOperationSupportSelection
printOperationSupportSelectionPassive :: PrintOperationClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
printOperationSupportSelectionPassive w = passivePropertyNE w printOperationSupportSelection


-- @A: printOperationTrackPrintStatus
printOperationTrackPrintStatusPassive :: PrintOperationClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
printOperationTrackPrintStatusPassive w = passivePropertyNE w printOperationTrackPrintStatus


-- @A: printOperationUnit
printOperationUnitPassive :: PrintOperationClass self => (self) -> ReactiveFieldReadWrite IO (Unit)
printOperationUnitPassive w = passivePropertyNE w printOperationUnit


-- @A: printOperationUseFullPage
printOperationUseFullPagePassive :: PrintOperationClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
printOperationUseFullPagePassive w = passivePropertyNE w printOperationUseFullPage


-- @S: printOptBeginPrint
-- TODO
-- @S: printOptCreateCustomWidget
-- TODO
-- @S: printOptCustomWidgetApply
-- TODO
-- @S: printOptDone
-- TODO
-- @S: printOptDrawPage
-- TODO
-- @S: printOptEndPrint
-- TODO
-- @S: printOptGotPageSize
-- TODO
-- @S: printOptPaginate
-- TODO
-- @S: printOptPreview
-- TODO
-- @S: printOptReady
-- TODO
-- @S: printOptRequestPageSetup
-- TODO
-- @S: printOptStatusChanged
printOptStatusChangedReactive :: PrintOperationClass self => self -> ReactiveFieldRead IO ()
printOptStatusChangedReactive = (`reactiveSignalIO` printOptStatusChanged)


-- @S: printOptUpdateCustomWidget
-- TODO
-- @A: printSettingsCollate
printSettingsCollatePassive :: PrintSettingsClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
printSettingsCollatePassive w = passivePropertyNE w printSettingsCollate


-- @A: printSettingsDefaultSource
printSettingsDefaultSourcePassive :: (PrintSettingsClass self, GlibString string) => (self) -> ReactiveFieldReadWrite IO (string)
printSettingsDefaultSourcePassive w = passivePropertyNE w printSettingsDefaultSource


-- @A: printSettingsDither
printSettingsDitherPassive :: (PrintSettingsClass self, GlibString string) => (self) -> ReactiveFieldReadWrite IO (string)
printSettingsDitherPassive w = passivePropertyNE w printSettingsDither


-- @A: printSettingsDuplex
printSettingsDuplexPassive :: PrintSettingsClass self => (self) -> ReactiveFieldReadWrite IO (PrintDuplex)
printSettingsDuplexPassive w = passivePropertyNE w printSettingsDuplex


-- @A: printSettingsFinishings
printSettingsFinishingsPassive :: (PrintSettingsClass self, GlibString string) => (self) -> ReactiveFieldReadWrite IO (string)
printSettingsFinishingsPassive w = passivePropertyNE w printSettingsFinishings


-- @G: printSettingsGetBool					 | ["print","Settings","Get","Bool"]
-- TODO
-- @G: printSettingsGetDouble					 | ["print","Settings","Get","Double"]
-- TODO
-- @G: printSettingsGetDoubleWithDefault					 | ["print","Settings","Get","Double","With","Default"]
-- TODO
-- @G: printSettingsGet					 | ["print","Settings","Get"]
-- TODO
-- @G: printSettingsGetInt					 | ["print","Settings","Get","Int"]
-- TODO
-- @G: printSettingsGetIntWithDefault					 | ["print","Settings","Get","Int","With","Default"]
-- TODO
-- @G: printSettingsGetLength					 | ["print","Settings","Get","Length"]
-- TODO
-- @G: printSettingsGetPaperHeight					 | ["print","Settings","Get","Paper","Height"]
-- TODO
-- @G: printSettingsGetPaperWidth					 | ["print","Settings","Get","Paper","Width"]
-- TODO
-- @G: printSettingsGetResolutionX					 | ["print","Settings","Get","Resolution","X"]
printSettingsGetResolutionXPassive :: PrintSettingsClass self => (self) -> ReactiveFieldRead IO (Int)
printSettingsGetResolutionXPassive w = wrapMRPassive (printSettingsGetResolutionX w)


-- @G: printSettingsGetResolutionY					 | ["print","Settings","Get","Resolution","Y"]
printSettingsGetResolutionYPassive :: PrintSettingsClass self => (self) -> ReactiveFieldRead IO (Int)
printSettingsGetResolutionYPassive w = wrapMRPassive (printSettingsGetResolutionY w)


-- @A: printSettingsMediaType
printSettingsMediaTypePassive :: (PrintSettingsClass self, GlibString string) => (self) -> ReactiveFieldReadWrite IO (string)
printSettingsMediaTypePassive w = passivePropertyNE w printSettingsMediaType


-- @A: printSettingsNCopies
printSettingsNCopiesPassive :: PrintSettingsClass self => (self) -> ReactiveFieldReadWrite IO (Int)
printSettingsNCopiesPassive w = passivePropertyNE w printSettingsNCopies


-- @A: printSettingsNumberUp
printSettingsNumberUpPassive :: PrintSettingsClass self => (self) -> ReactiveFieldReadWrite IO (Int)
printSettingsNumberUpPassive w = passivePropertyNE w printSettingsNumberUp


-- @A: printSettingsNumberUpLayout
printSettingsNumberUpLayoutPassive :: PrintSettingsClass self => (self) -> ReactiveFieldReadWrite IO (NumberUpLayout)
printSettingsNumberUpLayoutPassive w = passivePropertyNE w printSettingsNumberUpLayout


-- @A: printSettingsOrientation
printSettingsOrientationPassive :: PrintSettingsClass self => (self) -> ReactiveFieldReadWrite IO (PageOrientation)
printSettingsOrientationPassive w = passivePropertyNE w printSettingsOrientation


-- @A: printSettingsOutputBin
printSettingsOutputBinPassive :: (PrintSettingsClass self, GlibString string) => (self) -> ReactiveFieldReadWrite IO (string)
printSettingsOutputBinPassive w = passivePropertyNE w printSettingsOutputBin


-- @A: printSettingsPageSet
printSettingsPageSetPassive :: PrintSettingsClass self => (self) -> ReactiveFieldReadWrite IO (PageSet)
printSettingsPageSetPassive w = passivePropertyNE w printSettingsPageSet


-- @A: printSettingsPaperSize
printSettingsPaperSizePassive :: PrintSettingsClass self => (self) -> ReactiveFieldReadWrite IO (PaperSize)
printSettingsPaperSizePassive w = passivePropertyNE w printSettingsPaperSize


-- @A: printSettingsPrinter
printSettingsPrinterPassive :: (PrintSettingsClass self, GlibString string) => (self) -> ReactiveFieldReadWrite IO (string)
printSettingsPrinterPassive w = passivePropertyNE w printSettingsPrinter


-- @A: printSettingsPrinterLpi
printSettingsPrinterLpiPassive :: PrintSettingsClass self => (self) -> ReactiveFieldReadWrite IO (Double)
printSettingsPrinterLpiPassive w = passivePropertyNE w printSettingsPrinterLpi


-- @A: printSettingsPrintPages
printSettingsPrintPagesPassive :: PrintSettingsClass self => (self) -> ReactiveFieldReadWrite IO (PrintPages)
printSettingsPrintPagesPassive w = passivePropertyNE w printSettingsPrintPages


-- @A: printSettingsQuality
printSettingsQualityPassive :: PrintSettingsClass self => (self) -> ReactiveFieldReadWrite IO (PrintQuality)
printSettingsQualityPassive w = passivePropertyNE w printSettingsQuality


-- @A: printSettingsResolution
printSettingsResolutionPassive :: PrintSettingsClass self => (self) -> ReactiveFieldReadWrite IO (Int)
printSettingsResolutionPassive w = passivePropertyNE w printSettingsResolution


-- @A: printSettingsReverse
printSettingsReversePassive :: PrintSettingsClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
printSettingsReversePassive w = passivePropertyNE w printSettingsReverse


-- @A: printSettingsScale
printSettingsScalePassive :: PrintSettingsClass self => (self) -> ReactiveFieldReadWrite IO (Double)
printSettingsScalePassive w = passivePropertyNE w printSettingsScale


-- @T: printSettingsSetBool					 | ["print","Settings","Set","Bool"]
-- TODO
-- @T: printSettingsSetDouble					 | ["print","Settings","Set","Double"]
-- TODO
-- @T: printSettingsSet					 | ["print","Settings","Set"]
-- TODO
-- @T: printSettingsSetInt					 | ["print","Settings","Set","Int"]
-- TODO
-- @T: printSettingsSetLength					 | ["print","Settings","Set","Length"]
-- TODO
-- @T: printSettingsSetPaperHeight					 | ["print","Settings","Set","Paper","Height"]
-- TODO
-- @T: printSettingsSetPaperWidth					 | ["print","Settings","Set","Paper","Width"]
-- TODO
-- @T: printSettingsSetResolutionXy					 | ["print","Settings","Set","Resolution","Xy"]
-- TODO
-- @A: printSettingsUseColor
printSettingsUseColorPassive :: PrintSettingsClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
printSettingsUseColorPassive w = passivePropertyNE w printSettingsUseColor


-- @A: recentChooserMenuShowNumbers
recentChooserMenuShowNumbersPassive :: RecentChooserMenuClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
recentChooserMenuShowNumbersPassive w = passivePropertyNE w recentChooserMenuShowNumbers


-- @G: recentChooserGetCurrentItem					 | ["recent","Chooser","Get","Current","Item"]
recentChooserGetCurrentItemPassive :: RecentChooserClass self => (self) -> ReactiveFieldRead IO (RecentInfo)
recentChooserGetCurrentItemPassive w = wrapMRPassive (recentChooserGetCurrentItem w)


-- @G: recentChooserGetCurrentURI					 | ["recent","Chooser","Get","Current","URI"]
recentChooserGetCurrentURIPassive :: (RecentChooserClass self, GlibString string) => (self) -> ReactiveFieldRead IO (string)
recentChooserGetCurrentURIPassive w = wrapMRPassive (recentChooserGetCurrentURI w)


-- @G: recentChooserGetItems					 | ["recent","Chooser","Get","Items"]
recentChooserGetItemsPassive :: RecentChooserClass self => (self) -> ReactiveFieldRead IO ([RecentInfo])
recentChooserGetItemsPassive w = wrapMRPassive (recentChooserGetItems w)


-- @G: recentChooserGetURIs					 | ["recent","Chooser","Get","URIs"]
recentChooserGetURIsPassive :: (RecentChooserClass self, GlibString string) => (self) -> ReactiveFieldRead IO ([string])
recentChooserGetURIsPassive w = wrapMRPassive (recentChooserGetURIs w)


-- @S: recentChooserItemActivated
recentChooserItemActivatedReactive :: RecentChooserClass self => self -> ReactiveFieldRead IO ()
recentChooserItemActivatedReactive = (`reactiveSignalIO` recentChooserItemActivated)


-- @A: recentChooserLimit
recentChooserLimitPassive :: RecentChooserClass self => (self) -> ReactiveFieldReadWrite IO (Int)
recentChooserLimitPassive w = passivePropertyNE w recentChooserLimit


-- @A: recentChooserLocalOnly
recentChooserLocalOnlyPassive :: RecentChooserClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
recentChooserLocalOnlyPassive w = passivePropertyNE w recentChooserLocalOnly


-- @S: recentChooserSelectionChanged
recentChooserSelectionChangedReactive :: RecentChooserClass self => self -> ReactiveFieldRead IO ()
recentChooserSelectionChangedReactive = (`reactiveSignalIO` recentChooserSelectionChanged)


-- @A: recentChooserSelectMultiple
recentChooserSelectMultiplePassive :: RecentChooserClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
recentChooserSelectMultiplePassive w = passivePropertyNE w recentChooserSelectMultiple


-- @T: recentChooserSetCurrentURI					 | ["recent","Chooser","Set","Current","URI"]
recentChooserSetCurrentURIReactive :: (RecentChooserClass self, GlibString string) => (self) -> ReactiveFieldWrite IO (string)
recentChooserSetCurrentURIReactive w = wrapMW (void . recentChooserSetCurrentURI w)

-- @T: recentChooserSetSortFunc					 | ["recent","Chooser","Set","Sort","Func"]
recentChooserSetSortFuncReactive :: RecentChooserClass self => (self) -> ReactiveFieldWrite IO (Maybe (RecentInfo -> IO Int))
recentChooserSetSortFuncReactive w = wrapMW (recentChooserSetSortFunc w)


-- @A: recentChooserShowIcons
recentChooserShowIconsPassive :: RecentChooserClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
recentChooserShowIconsPassive w = passivePropertyNE w recentChooserShowIcons


-- @A: recentChooserShowNotFound
recentChooserShowNotFoundPassive :: RecentChooserClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
recentChooserShowNotFoundPassive w = passivePropertyNE w recentChooserShowNotFound


-- @A: recentChooserShowPrivate
recentChooserShowPrivatePassive :: RecentChooserClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
recentChooserShowPrivatePassive w = passivePropertyNE w recentChooserShowPrivate


-- @A: recentChooserShowTips
recentChooserShowTipsPassive :: RecentChooserClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
recentChooserShowTipsPassive w = passivePropertyNE w recentChooserShowTips


-- @A: recentChooserSortType
recentChooserSortTypePassive :: RecentChooserClass self => (self) -> ReactiveFieldReadWrite IO (RecentSortType)
recentChooserSortTypePassive w = passivePropertyNE w recentChooserSortType


-- @G: recentFilterGetName					 | ["recent","Filter","Get","Name"]
recentFilterGetNamePassive :: (RecentFilterClass self, GlibString string) => (self) -> ReactiveFieldRead IO (string)
recentFilterGetNamePassive w = wrapMRPassive (recentFilterGetName w)


-- @T: recentFilterSetName					 | ["recent","Filter","Set","Name"]
recentFilterSetNameReactive :: (RecentFilterClass self, GlibString string) => (self) -> ReactiveFieldWrite IO (string)
recentFilterSetNameReactive w = wrapMW (recentFilterSetName w)


-- @G: recentInfoGetAdded					 | ["recent","Info","Get","Added"]
recentInfoGetAddedPassive :: (RecentInfo) -> ReactiveFieldRead IO (Int)
recentInfoGetAddedPassive w = wrapMRPassive (recentInfoGetAdded w)


-- @G: recentInfoGetAge					 | ["recent","Info","Get","Age"]
recentInfoGetAgePassive :: (RecentInfo) -> ReactiveFieldRead IO (Int)
recentInfoGetAgePassive w = wrapMRPassive (recentInfoGetAge w)


-- @G: recentInfoGetApplicationInfo					 | ["recent","Info","Get","Application","Info"]
-- TODO
-- @G: recentInfoGetApplications					 | ["recent","Info","Get","Applications"]
recentInfoGetApplicationsPassive :: GlibString string => (RecentInfo) -> ReactiveFieldRead IO ([string])
recentInfoGetApplicationsPassive w = wrapMRPassive (recentInfoGetApplications w)


-- @G: recentInfoGetDescription					 | ["recent","Info","Get","Description"]
recentInfoGetDescriptionPassive :: GlibString string => (RecentInfo) -> ReactiveFieldRead IO (string)
recentInfoGetDescriptionPassive w = wrapMRPassive (recentInfoGetDescription w)


-- @G: recentInfoGetDisplayName					 | ["recent","Info","Get","Display","Name"]
recentInfoGetDisplayNamePassive :: GlibString string => (RecentInfo) -> ReactiveFieldRead IO (string)
recentInfoGetDisplayNamePassive w = wrapMRPassive (recentInfoGetDisplayName w)


-- @G: recentInfoGetGroups					 | ["recent","Info","Get","Groups"]
recentInfoGetGroupsPassive :: GlibString string => (RecentInfo) -> ReactiveFieldRead IO ([string])
recentInfoGetGroupsPassive w = wrapMRPassive (recentInfoGetGroups w)


-- @G: recentInfoGetIcon					 | ["recent","Info","Get","Icon"]
-- TODO
-- @G: recentInfoGetMimeType					 | ["recent","Info","Get","Mime","Type"]
recentInfoGetMimeTypePassive :: GlibString string => (RecentInfo) -> ReactiveFieldRead IO (string)
recentInfoGetMimeTypePassive w = wrapMRPassive (recentInfoGetMimeType w)


-- @G: recentInfoGetModified					 | ["recent","Info","Get","Modified"]
recentInfoGetModifiedPassive :: (RecentInfo) -> ReactiveFieldRead IO (Int)
recentInfoGetModifiedPassive w = wrapMRPassive (recentInfoGetModified w)


-- @G: recentInfoGetPrivateHint					 | ["recent","Info","Get","Private","Hint"]
recentInfoGetPrivateHintPassive :: (RecentInfo) -> ReactiveFieldRead IO (Bool)
recentInfoGetPrivateHintPassive w = wrapMRPassive (recentInfoGetPrivateHint w)


-- @G: recentInfoGetShortName					 | ["recent","Info","Get","Short","Name"]
recentInfoGetShortNamePassive :: GlibString string => (RecentInfo) -> ReactiveFieldRead IO (string)
recentInfoGetShortNamePassive w = wrapMRPassive (recentInfoGetShortName w)


-- @G: recentInfoGetURIDisplay					 | ["recent","Info","Get","URIDisplay"]
recentInfoGetURIDisplayPassive :: GlibString string => (RecentInfo) -> ReactiveFieldRead IO (string)
recentInfoGetURIDisplayPassive w = wrapMRPassive (recentInfoGetURIDisplay w)


-- @G: recentInfoGetURI					 | ["recent","Info","Get","URI"]
recentInfoGetURIPassive :: GlibString string => (RecentInfo) -> ReactiveFieldRead IO (string)
recentInfoGetURIPassive w = wrapMRPassive (recentInfoGetURI w)


-- @G: recentInfoGetVisited					 | ["recent","Info","Get","Visited"]
recentInfoGetVisitedPassive :: (RecentInfo) -> ReactiveFieldRead IO (Int)
recentInfoGetVisitedPassive w = wrapMRPassive (recentInfoGetVisited w)


-- @S: recentManagerChanged
recentManagerChangedReactive :: RecentManagerClass self => self -> ReactiveFieldRead IO ()
recentManagerChangedReactive = (`reactiveSignalIO` recentManagerChanged)


-- @G: recentManagerGetDefault					 | ["recent","Manager","Get","Default"]
recentManagerGetDefaultPassive :: ReactiveFieldRead IO (RecentManager)
recentManagerGetDefaultPassive = wrapMRPassive (recentManagerGetDefault)


-- @G: recentManagerGetItems					 | ["recent","Manager","Get","Items"]
recentManagerGetItemsPassive :: RecentManagerClass self => (self) -> ReactiveFieldRead IO ([RecentInfo])
recentManagerGetItemsPassive w = wrapMRPassive (recentManagerGetItems w)


-- @A: recentManagerLimit
recentManagerLimitPassive :: RecentManagerClass self => (self) -> ReactiveFieldReadWrite IO (Int)
recentManagerLimitPassive w = passivePropertyNE w recentManagerLimit


-- @G: scrolledWindowGetHAdjustment					 | ["scrolled","Window","Get","HAdjustment"]
scrolledWindowGetHAdjustmentPassive :: ScrolledWindowClass self => (self) -> ReactiveFieldRead IO (Adjustment)
scrolledWindowGetHAdjustmentPassive w = wrapMRPassive (scrolledWindowGetHAdjustment w)


-- @G: scrolledWindowGetHScrollbar					 | ["scrolled","Window","Get","HScrollbar"]
scrolledWindowGetHScrollbarPassive :: ScrolledWindowClass self => (self) -> ReactiveFieldRead IO ((Maybe HScrollbar))
scrolledWindowGetHScrollbarPassive w = wrapMRPassive (scrolledWindowGetHScrollbar w)


-- @G: scrolledWindowGetPlacement					 | ["scrolled","Window","Get","Placement"]
scrolledWindowGetPlacementPassive :: ScrolledWindowClass self => (self) -> ReactiveFieldRead IO (CornerType)
scrolledWindowGetPlacementPassive w = wrapMRPassive (scrolledWindowGetPlacement w)


-- @G: scrolledWindowGetPolicy					 | ["scrolled","Window","Get","Policy"]
scrolledWindowGetPolicyPassive :: ScrolledWindowClass self => (self) -> ReactiveFieldRead IO ((PolicyType, PolicyType))
scrolledWindowGetPolicyPassive w = wrapMRPassive (scrolledWindowGetPolicy w)


-- @G: scrolledWindowGetShadowType					 | ["scrolled","Window","Get","Shadow","Type"]
scrolledWindowGetShadowTypePassive :: ScrolledWindowClass self => (self) -> ReactiveFieldRead IO (ShadowType)
scrolledWindowGetShadowTypePassive w = wrapMRPassive (scrolledWindowGetShadowType w)


-- @G: scrolledWindowGetVAdjustment					 | ["scrolled","Window","Get","VAdjustment"]
scrolledWindowGetVAdjustmentPassive :: ScrolledWindowClass self => (self) -> ReactiveFieldRead IO (Adjustment)
scrolledWindowGetVAdjustmentPassive w = wrapMRPassive (scrolledWindowGetVAdjustment w)


-- @G: scrolledWindowGetVScrollbar					 | ["scrolled","Window","Get","VScrollbar"]
scrolledWindowGetVScrollbarPassive :: ScrolledWindowClass self => (self) -> ReactiveFieldRead IO ((Maybe VScrollbar))
scrolledWindowGetVScrollbarPassive w = wrapMRPassive (scrolledWindowGetVScrollbar w)


-- @A: scrolledWindowHAdjustment
scrolledWindowHAdjustmentPassive :: ScrolledWindowClass self => (self) -> ReactiveFieldReadWrite IO (Adjustment)
scrolledWindowHAdjustmentPassive w = passivePropertyNE w scrolledWindowHAdjustment


-- @A: scrolledWindowHscrollbarPolicy
scrolledWindowHscrollbarPolicyPassive :: ScrolledWindowClass self => (self) -> ReactiveFieldReadWrite IO (PolicyType)
scrolledWindowHscrollbarPolicyPassive w = passivePropertyNE w scrolledWindowHscrollbarPolicy


-- @A: scrolledWindowPlacement
scrolledWindowPlacementPassive :: ScrolledWindowClass self => (self) -> ReactiveFieldReadWrite IO (CornerType)
scrolledWindowPlacementPassive w = passivePropertyNE w scrolledWindowPlacement


-- @T: scrolledWindowSetHAdjustment					 | ["scrolled","Window","Set","HAdjustment"]
scrolledWindowSetHAdjustmentReactive :: ScrolledWindowClass self => (self) -> ReactiveFieldWrite IO (Adjustment)
scrolledWindowSetHAdjustmentReactive w = wrapMW (scrolledWindowSetHAdjustment w)


-- @T: scrolledWindowSetPlacement					 | ["scrolled","Window","Set","Placement"]
scrolledWindowSetPlacementReactive :: ScrolledWindowClass self => (self) -> ReactiveFieldWrite IO (CornerType)
scrolledWindowSetPlacementReactive w = wrapMW (scrolledWindowSetPlacement w)


-- @T: scrolledWindowSetPolicy					 | ["scrolled","Window","Set","Policy"]
-- TODO
-- @T: scrolledWindowSetShadowType					 | ["scrolled","Window","Set","Shadow","Type"]
scrolledWindowSetShadowTypeReactive :: ScrolledWindowClass self => (self) -> ReactiveFieldWrite IO (ShadowType)
scrolledWindowSetShadowTypeReactive w = wrapMW (scrolledWindowSetShadowType w)


-- @T: scrolledWindowSetVAdjustment					 | ["scrolled","Window","Set","VAdjustment"]
scrolledWindowSetVAdjustmentReactive :: ScrolledWindowClass self => (self) -> ReactiveFieldWrite IO (Adjustment)
scrolledWindowSetVAdjustmentReactive w = wrapMW (scrolledWindowSetVAdjustment w)


-- @A: scrolledWindowShadowType
scrolledWindowShadowTypePassive :: ScrolledWindowClass self => (self) -> ReactiveFieldReadWrite IO (ShadowType)
scrolledWindowShadowTypePassive w = passivePropertyNE w scrolledWindowShadowType


-- @A: scrolledWindowVAdjustment
scrolledWindowVAdjustmentPassive :: ScrolledWindowClass self => (self) -> ReactiveFieldReadWrite IO (Adjustment)
scrolledWindowVAdjustmentPassive w = passivePropertyNE w scrolledWindowVAdjustment


-- @A: scrolledWindowVscrollbarPolicy
scrolledWindowVscrollbarPolicyPassive :: ScrolledWindowClass self => (self) -> ReactiveFieldReadWrite IO (PolicyType)
scrolledWindowVscrollbarPolicyPassive w = passivePropertyNE w scrolledWindowVscrollbarPolicy


-- @A: scrolledWindowWindowPlacement
scrolledWindowWindowPlacementPassive :: ScrolledWindowClass self => (self) -> ReactiveFieldReadWrite IO (CornerType)
scrolledWindowWindowPlacementPassive w = passivePropertyNE w scrolledWindowWindowPlacement


-- @C: afterColorSet
afterColorSetReactive :: ColorButtonClass self => self -> ReactiveFieldRead IO ()
afterColorSetReactive w = reactivePropertyH_ w afterColorSet

-- @A: colorButtonAlpha
colorButtonAlphaPassive :: ColorButtonClass self => (self) -> ReactiveFieldReadWrite IO (Word16)
colorButtonAlphaPassive w = passivePropertyNE w colorButtonAlpha


-- @G: colorButtonGetAlpha					 | ["color","Button","Get","Alpha"]
colorButtonGetAlphaPassive :: ColorButtonClass self => (self) -> ReactiveFieldRead IO (Word16)
colorButtonGetAlphaPassive w = wrapMRPassive (colorButtonGetAlpha w)


-- @G: colorButtonGetColor					 | ["color","Button","Get","Color"]
colorButtonGetColorPassive :: ColorButtonClass self => (self) -> ReactiveFieldRead IO (Color)
colorButtonGetColorPassive w = wrapMRPassive (colorButtonGetColor w)


-- @G: colorButtonGetTitle					 | ["color","Button","Get","Title"]
colorButtonGetTitlePassive :: (ColorButtonClass self, GlibString string) => (self) -> ReactiveFieldRead IO (string)
colorButtonGetTitlePassive w = wrapMRPassive (colorButtonGetTitle w)


-- @G: colorButtonGetUseAlpha					 | ["color","Button","Get","Use","Alpha"]
colorButtonGetUseAlphaPassive :: ColorButtonClass self => (self) -> ReactiveFieldRead IO (Bool)
colorButtonGetUseAlphaPassive w = wrapMRPassive (colorButtonGetUseAlpha w)


-- @T: colorButtonSetAlpha					 | ["color","Button","Set","Alpha"]
colorButtonSetAlphaReactive :: ColorButtonClass self => (self) -> ReactiveFieldWrite IO (Word16)
colorButtonSetAlphaReactive w = wrapMW (colorButtonSetAlpha w)


-- @T: colorButtonSetColor					 | ["color","Button","Set","Color"]
colorButtonSetColorReactive :: ColorButtonClass self => (self) -> ReactiveFieldWrite IO (Color)
colorButtonSetColorReactive w = wrapMW (colorButtonSetColor w)


-- @T: colorButtonSetTitle					 | ["color","Button","Set","Title"]
colorButtonSetTitleReactive :: (ColorButtonClass self, GlibString string) => (self) -> ReactiveFieldWrite IO (string)
colorButtonSetTitleReactive w = wrapMW (colorButtonSetTitle w)


-- @T: colorButtonSetUseAlpha					 | ["color","Button","Set","Use","Alpha"]
colorButtonSetUseAlphaReactive :: ColorButtonClass self => (self) -> ReactiveFieldWrite IO (Bool)
colorButtonSetUseAlphaReactive w = wrapMW (colorButtonSetUseAlpha w)


-- @A: colorButtonTitle
colorButtonTitlePassive :: (ColorButtonClass self, GlibString string) => (self) -> ReactiveFieldReadWrite IO (string)
colorButtonTitlePassive w = passivePropertyNE w colorButtonTitle


-- @A: colorButtonUseAlpha
colorButtonUseAlphaPassive :: ColorButtonClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
colorButtonUseAlphaPassive w = passivePropertyNE w colorButtonUseAlpha


-- @C: onColorSet
onColorSetReactive :: ColorButtonClass self => self -> ReactiveFieldRead IO ()
onColorSetReactive w = reactivePropertyH_ w onColorSet

-- @A: colorSelectionCurrentAlpha
colorSelectionCurrentAlphaPassive :: ColorSelectionClass self => (self) -> ReactiveFieldReadWrite IO (Int)
colorSelectionCurrentAlphaPassive w = passivePropertyNE w colorSelectionCurrentAlpha


-- @G: colorSelectionGetCurrentAlpha					 | ["color","Selection","Get","Current","Alpha"]
colorSelectionGetCurrentAlphaPassive :: ColorSelectionClass self => (self) -> ReactiveFieldRead IO (Int)
colorSelectionGetCurrentAlphaPassive w = wrapMRPassive (colorSelectionGetCurrentAlpha w)


-- @G: colorSelectionGetCurrentColor					 | ["color","Selection","Get","Current","Color"]
colorSelectionGetCurrentColorPassive :: ColorSelectionClass self => (self) -> ReactiveFieldRead IO (Color)
colorSelectionGetCurrentColorPassive w = wrapMRPassive (colorSelectionGetCurrentColor w)


-- @G: colorSelectionGetHasOpacityControl					 | ["color","Selection","Get","Has","Opacity","Control"]
colorSelectionGetHasOpacityControlPassive :: ColorSelectionClass self => (self) -> ReactiveFieldRead IO (Bool)
colorSelectionGetHasOpacityControlPassive w = wrapMRPassive (colorSelectionGetHasOpacityControl w)


-- @G: colorSelectionGetHasPalette					 | ["color","Selection","Get","Has","Palette"]
colorSelectionGetHasPalettePassive :: ColorSelectionClass self => (self) -> ReactiveFieldRead IO (Bool)
colorSelectionGetHasPalettePassive w = wrapMRPassive (colorSelectionGetHasPalette w)


-- @G: colorSelectionGetPreviousAlpha					 | ["color","Selection","Get","Previous","Alpha"]
colorSelectionGetPreviousAlphaPassive :: ColorSelectionClass self => (self) -> ReactiveFieldRead IO (Int)
colorSelectionGetPreviousAlphaPassive w = wrapMRPassive (colorSelectionGetPreviousAlpha w)


-- @G: colorSelectionGetPreviousColor					 | ["color","Selection","Get","Previous","Color"]
colorSelectionGetPreviousColorPassive :: ColorSelectionClass self => (self) -> ReactiveFieldRead IO (Color)
colorSelectionGetPreviousColorPassive w = wrapMRPassive (colorSelectionGetPreviousColor w)


-- @A: colorSelectionHasOpacityControl
colorSelectionHasOpacityControlPassive :: ColorSelectionClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
colorSelectionHasOpacityControlPassive w = passivePropertyNE w colorSelectionHasOpacityControl


-- @A: colorSelectionHasPalette
colorSelectionHasPalettePassive :: ColorSelectionClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
colorSelectionHasPalettePassive w = passivePropertyNE w colorSelectionHasPalette


-- @A: colorSelectionPreviousAlpha
colorSelectionPreviousAlphaPassive :: ColorSelectionClass self => (self) -> ReactiveFieldReadWrite IO (Int)
colorSelectionPreviousAlphaPassive w = passivePropertyNE w colorSelectionPreviousAlpha


-- @T: colorSelectionSetCurrentAlpha					 | ["color","Selection","Set","Current","Alpha"]
colorSelectionSetCurrentAlphaReactive :: ColorSelectionClass self => (self) -> ReactiveFieldWrite IO (Int)
colorSelectionSetCurrentAlphaReactive w = wrapMW (colorSelectionSetCurrentAlpha w)


-- @T: colorSelectionSetCurrentColor					 | ["color","Selection","Set","Current","Color"]
colorSelectionSetCurrentColorReactive :: ColorSelectionClass self => (self) -> ReactiveFieldWrite IO (Color)
colorSelectionSetCurrentColorReactive w = wrapMW (colorSelectionSetCurrentColor w)


-- @T: colorSelectionSetHasOpacityControl					 | ["color","Selection","Set","Has","Opacity","Control"]
colorSelectionSetHasOpacityControlReactive :: ColorSelectionClass self => (self) -> ReactiveFieldWrite IO (Bool)
colorSelectionSetHasOpacityControlReactive w = wrapMW (colorSelectionSetHasOpacityControl w)


-- @T: colorSelectionSetHasPalette					 | ["color","Selection","Set","Has","Palette"]
colorSelectionSetHasPaletteReactive :: ColorSelectionClass self => (self) -> ReactiveFieldWrite IO (Bool)
colorSelectionSetHasPaletteReactive w = wrapMW (colorSelectionSetHasPalette w)


-- @T: colorSelectionSetPreviousAlpha					 | ["color","Selection","Set","Previous","Alpha"]
colorSelectionSetPreviousAlphaReactive :: ColorSelectionClass self => (self) -> ReactiveFieldWrite IO (Int)
colorSelectionSetPreviousAlphaReactive w = wrapMW (colorSelectionSetPreviousAlpha w)


-- @T: colorSelectionSetPreviousColor					 | ["color","Selection","Set","Previous","Color"]
colorSelectionSetPreviousColorReactive :: ColorSelectionClass self => (self) -> ReactiveFieldWrite IO (Color)
colorSelectionSetPreviousColorReactive w = wrapMW (colorSelectionSetPreviousColor w)


-- @C: afterConfirmOverwrite
-- TODO
-- @C: afterCurrentFolderChanged
afterCurrentFolderChangedReactive :: FileChooserClass self => self -> ReactiveFieldRead IO ()
afterCurrentFolderChangedReactive w = reactivePropertyH_ w afterCurrentFolderChanged

-- @C: afterFileActivated
afterFileActivatedReactive :: FileChooserClass self => self -> ReactiveFieldRead IO ()
afterFileActivatedReactive w = reactivePropertyH_ w afterFileActivated

-- @C: afterUpdatePreview
afterUpdatePreviewReactive :: FileChooserClass self => self -> ReactiveFieldRead IO ()
afterUpdatePreviewReactive w = reactivePropertyH_ w afterUpdatePreview

-- @G: fileChooserButtonGetTitle					 | ["file","Chooser","Button","Get","Title"]
fileChooserButtonGetTitlePassive :: (FileChooserButtonClass self, GlibString string) => (self) -> ReactiveFieldRead IO (string)
fileChooserButtonGetTitlePassive w = wrapMRPassive (fileChooserButtonGetTitle w)


-- @G: fileChooserButtonGetWidthChars					 | ["file","Chooser","Button","Get","Width","Chars"]
fileChooserButtonGetWidthCharsPassive :: FileChooserButtonClass self => (self) -> ReactiveFieldRead IO (Int)
fileChooserButtonGetWidthCharsPassive w = wrapMRPassive (fileChooserButtonGetWidthChars w)


-- @T: fileChooserButtonSetTitle					 | ["file","Chooser","Button","Set","Title"]
fileChooserButtonSetTitleReactive :: (FileChooserButtonClass self, GlibString string) => (self) -> ReactiveFieldWrite IO (string)
fileChooserButtonSetTitleReactive w = wrapMW (fileChooserButtonSetTitle w)


-- @T: fileChooserButtonSetWidthChars					 | ["file","Chooser","Button","Set","Width","Chars"]
fileChooserButtonSetWidthCharsReactive :: FileChooserButtonClass self => (self) -> ReactiveFieldWrite IO (Int)
fileChooserButtonSetWidthCharsReactive w = wrapMW (fileChooserButtonSetWidthChars w)


-- @A: fileChooserButtonTitle
fileChooserButtonTitlePassive :: (FileChooserButtonClass self, GlibString string) => (self) -> ReactiveFieldReadWrite IO (string)
fileChooserButtonTitlePassive w = passivePropertyNE w fileChooserButtonTitle


-- @A: fileChooserButtonWidthChars
fileChooserButtonWidthCharsPassive :: FileChooserButtonClass self => (self) -> ReactiveFieldReadWrite IO (Int)
fileChooserButtonWidthCharsPassive w = passivePropertyNE w fileChooserButtonWidthChars


-- @S: confirmOverwrite
-- TODO
-- @S: currentFolderChanged
currentFolderChangedReactive :: FileChooserClass self => self -> ReactiveFieldRead IO ()
currentFolderChangedReactive = (`reactiveSignalIO` currentFolderChanged)


-- @S: fileActivated
fileActivatedReactive :: FileChooserClass self => self -> ReactiveFieldRead IO ()
fileActivatedReactive = (`reactiveSignalIO` fileActivated)


-- @A: fileChooserAction
fileChooserActionPassive :: FileChooserClass self => (self) -> ReactiveFieldReadWrite IO (FileChooserAction)
fileChooserActionPassive w = passivePropertyNE w fileChooserAction


-- @A: fileChooserDoOverwriteConfirmation
fileChooserDoOverwriteConfirmationPassive :: FileChooserClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
fileChooserDoOverwriteConfirmationPassive w = passivePropertyNE w fileChooserDoOverwriteConfirmation


-- @G: fileChooserGetAction					 | ["file","Chooser","Get","Action"]
fileChooserGetActionPassive :: FileChooserClass self => (self) -> ReactiveFieldRead IO (FileChooserAction)
fileChooserGetActionPassive w = wrapMRPassive (fileChooserGetAction w)


-- @G: fileChooserGetCurrentFolder					 | ["file","Chooser","Get","Current","Folder"]
fileChooserGetCurrentFolderPassive :: FileChooserClass self => (self) -> ReactiveFieldRead IO ((Maybe FilePath))
fileChooserGetCurrentFolderPassive w = wrapMRPassive (fileChooserGetCurrentFolder w)


-- @G: fileChooserGetCurrentFolderURI					 | ["file","Chooser","Get","Current","Folder","URI"]
fileChooserGetCurrentFolderURIPassive :: FileChooserClass self => (self) -> ReactiveFieldRead IO (String)
fileChooserGetCurrentFolderURIPassive w = wrapMRPassive (fileChooserGetCurrentFolderURI w)


-- @G: fileChooserGetDoOverwriteConfirmation					 | ["file","Chooser","Get","Do","Overwrite","Confirmation"]
fileChooserGetDoOverwriteConfirmationPassive :: FileChooserClass self => (self) -> ReactiveFieldRead IO (Bool)
fileChooserGetDoOverwriteConfirmationPassive w = wrapMRPassive (fileChooserGetDoOverwriteConfirmation w)


-- @G: fileChooserGetExtraWidget					 | ["file","Chooser","Get","Extra","Widget"]
fileChooserGetExtraWidgetPassive :: FileChooserClass self => (self) -> ReactiveFieldRead IO ((Maybe Widget))
fileChooserGetExtraWidgetPassive w = wrapMRPassive (fileChooserGetExtraWidget w)


-- @G: fileChooserGetFilename					 | ["file","Chooser","Get","Filename"]
fileChooserGetFilenamePassive :: (FileChooserClass self, GlibFilePath fp) => (self) -> ReactiveFieldRead IO ((Maybe fp))
fileChooserGetFilenamePassive w = wrapMRPassive (fileChooserGetFilename w)


-- @G: fileChooserGetFilenames					 | ["file","Chooser","Get","Filenames"]
fileChooserGetFilenamesPassive :: FileChooserClass self => (self) -> ReactiveFieldRead IO ([FilePath])
fileChooserGetFilenamesPassive w = wrapMRPassive (fileChooserGetFilenames w)


-- @G: fileChooserGetFilter					 | ["file","Chooser","Get","Filter"]
fileChooserGetFilterPassive :: FileChooserClass self => (self) -> ReactiveFieldRead IO ((Maybe FileFilter))
fileChooserGetFilterPassive w = wrapMRPassive (fileChooserGetFilter w)


-- @G: fileChooserGetLocalOnly					 | ["file","Chooser","Get","Local","Only"]
fileChooserGetLocalOnlyPassive :: FileChooserClass self => (self) -> ReactiveFieldRead IO (Bool)
fileChooserGetLocalOnlyPassive w = wrapMRPassive (fileChooserGetLocalOnly w)


-- @G: fileChooserGetPreviewFilename					 | ["file","Chooser","Get","Preview","Filename"]
fileChooserGetPreviewFilenamePassive :: FileChooserClass self => (self) -> ReactiveFieldRead IO ((Maybe FilePath))
fileChooserGetPreviewFilenamePassive w = wrapMRPassive (fileChooserGetPreviewFilename w)


-- @G: fileChooserGetPreviewURI					 | ["file","Chooser","Get","Preview","URI"]
fileChooserGetPreviewURIPassive :: FileChooserClass self => (self) -> ReactiveFieldRead IO ((Maybe String))
fileChooserGetPreviewURIPassive w = wrapMRPassive (fileChooserGetPreviewURI w)


-- @G: fileChooserGetPreviewWidgetActive					 | ["file","Chooser","Get","Preview","Widget","Active"]
fileChooserGetPreviewWidgetActivePassive :: FileChooserClass self => (self) -> ReactiveFieldRead IO (Bool)
fileChooserGetPreviewWidgetActivePassive w = wrapMRPassive (fileChooserGetPreviewWidgetActive w)


-- @G: fileChooserGetPreviewWidget					 | ["file","Chooser","Get","Preview","Widget"]
fileChooserGetPreviewWidgetPassive :: FileChooserClass self => (self) -> ReactiveFieldRead IO ((Maybe Widget))
fileChooserGetPreviewWidgetPassive w = wrapMRPassive (fileChooserGetPreviewWidget w)


-- @G: fileChooserGetSelectMultiple					 | ["file","Chooser","Get","Select","Multiple"]
fileChooserGetSelectMultiplePassive :: FileChooserClass self => (self) -> ReactiveFieldRead IO (Bool)
fileChooserGetSelectMultiplePassive w = wrapMRPassive (fileChooserGetSelectMultiple w)


-- @G: fileChooserGetShowHidden					 | ["file","Chooser","Get","Show","Hidden"]
fileChooserGetShowHiddenPassive :: FileChooserClass self => (self) -> ReactiveFieldRead IO (Bool)
fileChooserGetShowHiddenPassive w = wrapMRPassive (fileChooserGetShowHidden w)


-- @G: fileChooserGetURI					 | ["file","Chooser","Get","URI"]
fileChooserGetURIPassive :: FileChooserClass self => (self) -> ReactiveFieldRead IO ((Maybe String))
fileChooserGetURIPassive w = wrapMRPassive (fileChooserGetURI w)


-- @G: fileChooserGetURIs					 | ["file","Chooser","Get","URIs"]
fileChooserGetURIsPassive :: FileChooserClass self => (self) -> ReactiveFieldRead IO ([String])
fileChooserGetURIsPassive w = wrapMRPassive (fileChooserGetURIs w)


-- @G: fileChooserGetUsePreviewLabel					 | ["file","Chooser","Get","Use","Preview","Label"]
fileChooserGetUsePreviewLabelPassive :: FileChooserClass self => (self) -> ReactiveFieldRead IO (Bool)
fileChooserGetUsePreviewLabelPassive w = wrapMRPassive (fileChooserGetUsePreviewLabel w)


-- @A: fileChooserLocalOnly
fileChooserLocalOnlyPassive :: FileChooserClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
fileChooserLocalOnlyPassive w = passivePropertyNE w fileChooserLocalOnly


-- @A: fileChooserPreviewWidgetActive
fileChooserPreviewWidgetActivePassive :: FileChooserClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
fileChooserPreviewWidgetActivePassive w = passivePropertyNE w fileChooserPreviewWidgetActive


-- @A: fileChooserSelectMultiple
fileChooserSelectMultiplePassive :: FileChooserClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
fileChooserSelectMultiplePassive w = passivePropertyNE w fileChooserSelectMultiple


-- @T: fileChooserSetAction					 | ["file","Chooser","Set","Action"]
fileChooserSetActionReactive :: FileChooserClass self => (self) -> ReactiveFieldWrite IO (FileChooserAction)
fileChooserSetActionReactive w = wrapMW (fileChooserSetAction w)


-- @T: fileChooserSetCurrentFolder					 | ["file","Chooser","Set","Current","Folder"]
fileChooserSetCurrentFolderReactive :: FileChooserClass self => (self) -> ReactiveFieldWrite IO (FilePath)
fileChooserSetCurrentFolderReactive w = wrapMW (void . fileChooserSetCurrentFolder w)

-- @T: fileChooserSetCurrentFolderURI					 | ["file","Chooser","Set","Current","Folder","URI"]
fileChooserSetCurrentFolderURIReactive :: FileChooserClass self => (self) -> ReactiveFieldWrite IO (String)
fileChooserSetCurrentFolderURIReactive w = wrapMW (void . fileChooserSetCurrentFolderURI w)

-- @T: fileChooserSetCurrentName					 | ["file","Chooser","Set","Current","Name"]
fileChooserSetCurrentNameReactive :: (FileChooserClass self, GlibFilePath fp) => (self) -> ReactiveFieldWrite IO (fp)
fileChooserSetCurrentNameReactive w = wrapMW (fileChooserSetCurrentName w)


-- @T: fileChooserSetDoOverwriteConfirmation					 | ["file","Chooser","Set","Do","Overwrite","Confirmation"]
fileChooserSetDoOverwriteConfirmationReactive :: FileChooserClass self => (self) -> ReactiveFieldWrite IO (Bool)
fileChooserSetDoOverwriteConfirmationReactive w = wrapMW (fileChooserSetDoOverwriteConfirmation w)


-- @T: fileChooserSetExtraWidget					 | ["file","Chooser","Set","Extra","Widget"]
fileChooserSetExtraWidgetReactive :: (FileChooserClass self, WidgetClass extraWidget) => (self) -> ReactiveFieldWrite IO (extraWidget)
fileChooserSetExtraWidgetReactive w = wrapMW (fileChooserSetExtraWidget w)


-- @T: fileChooserSetFilename					 | ["file","Chooser","Set","Filename"]
fileChooserSetFilenameReactive :: FileChooserClass self => (self) -> ReactiveFieldWrite IO (FilePath)
fileChooserSetFilenameReactive w = wrapMW (void . fileChooserSetFilename w)

-- @T: fileChooserSetFilter					 | ["file","Chooser","Set","Filter"]
fileChooserSetFilterReactive :: FileChooserClass self => (self) -> ReactiveFieldWrite IO (FileFilter)
fileChooserSetFilterReactive w = wrapMW (fileChooserSetFilter w)


-- @T: fileChooserSetLocalOnly					 | ["file","Chooser","Set","Local","Only"]
fileChooserSetLocalOnlyReactive :: FileChooserClass self => (self) -> ReactiveFieldWrite IO (Bool)
fileChooserSetLocalOnlyReactive w = wrapMW (fileChooserSetLocalOnly w)


-- @T: fileChooserSetPreviewWidgetActive					 | ["file","Chooser","Set","Preview","Widget","Active"]
fileChooserSetPreviewWidgetActiveReactive :: FileChooserClass self => (self) -> ReactiveFieldWrite IO (Bool)
fileChooserSetPreviewWidgetActiveReactive w = wrapMW (fileChooserSetPreviewWidgetActive w)


-- @T: fileChooserSetPreviewWidget					 | ["file","Chooser","Set","Preview","Widget"]
fileChooserSetPreviewWidgetReactive :: (FileChooserClass self, WidgetClass previewWidget) => (self) -> ReactiveFieldWrite IO (previewWidget)
fileChooserSetPreviewWidgetReactive w = wrapMW (fileChooserSetPreviewWidget w)


-- @T: fileChooserSetSelectMultiple					 | ["file","Chooser","Set","Select","Multiple"]
fileChooserSetSelectMultipleReactive :: FileChooserClass self => (self) -> ReactiveFieldWrite IO (Bool)
fileChooserSetSelectMultipleReactive w = wrapMW (fileChooserSetSelectMultiple w)


-- @T: fileChooserSetShowHidden					 | ["file","Chooser","Set","Show","Hidden"]
fileChooserSetShowHiddenReactive :: FileChooserClass self => (self) -> ReactiveFieldWrite IO (Bool)
fileChooserSetShowHiddenReactive w = wrapMW (fileChooserSetShowHidden w)


-- @T: fileChooserSetURI					 | ["file","Chooser","Set","URI"]
fileChooserSetURIReactive :: FileChooserClass self => (self) -> ReactiveFieldWrite IO (String)
fileChooserSetURIReactive w = wrapMW (void . fileChooserSetURI w)

-- @T: fileChooserSetUsePreviewLabel					 | ["file","Chooser","Set","Use","Preview","Label"]
fileChooserSetUsePreviewLabelReactive :: FileChooserClass self => (self) -> ReactiveFieldWrite IO (Bool)
fileChooserSetUsePreviewLabelReactive w = wrapMW (fileChooserSetUsePreviewLabel w)


-- @A: fileChooserShowHidden
fileChooserShowHiddenPassive :: FileChooserClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
fileChooserShowHiddenPassive w = passivePropertyNE w fileChooserShowHidden


-- @A: fileChooserUsePreviewLabel
fileChooserUsePreviewLabelPassive :: FileChooserClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
fileChooserUsePreviewLabelPassive w = passivePropertyNE w fileChooserUsePreviewLabel


-- @S: fileSelectionChanged
fileSelectionChangedReactive :: FileChooserClass self => self -> ReactiveFieldRead IO ()
fileSelectionChangedReactive = (`reactiveSignalIO` fileSelectionChanged)


-- @C: onConfirmOverwrite
-- TODO
-- @C: onCurrentFolderChanged
onCurrentFolderChangedReactive :: FileChooserClass self => self -> ReactiveFieldRead IO ()
onCurrentFolderChangedReactive w = reactivePropertyH_ w onCurrentFolderChanged

-- @C: onFileActivated
onFileActivatedReactive :: FileChooserClass self => self -> ReactiveFieldRead IO ()
onFileActivatedReactive w = reactivePropertyH_ w onFileActivated

-- @C: onUpdatePreview
onUpdatePreviewReactive :: FileChooserClass self => self -> ReactiveFieldRead IO ()
onUpdatePreviewReactive w = reactivePropertyH_ w onUpdatePreview

-- @S: updatePreview
updatePreviewReactive :: FileChooserClass self => self -> ReactiveFieldRead IO ()
updatePreviewReactive = (`reactiveSignalIO` updatePreview)


-- @G: fileFilterGetName					 | ["file","Filter","Get","Name"]
fileFilterGetNamePassive :: GlibString string => (FileFilter) -> ReactiveFieldRead IO (string)
fileFilterGetNamePassive w = wrapMRPassive (fileFilterGetName w)


-- @A: fileFilterName
fileFilterNamePassive :: GlibString string => FileFilter -> ReactiveFieldReadWrite IO string
fileFilterNamePassive w = passivePropertyNE w fileFilterName


-- @T: fileFilterSetName					 | ["file","Filter","Set","Name"]
fileFilterSetNameReactive :: GlibString string => (FileFilter) -> ReactiveFieldWrite IO (string)
fileFilterSetNameReactive w = wrapMW (fileFilterSetName w)


-- @A: fileSelectionFilename
fileSelectionFilenamePassive :: (FileSelectionClass self, GlibString string) => (self) -> ReactiveFieldReadWrite IO (string)
fileSelectionFilenamePassive w = passivePropertyNE w fileSelectionFilename


-- @G: fileSelectionGetFilename					 | ["file","Selection","Get","Filename"]
fileSelectionGetFilenamePassive :: (FileSelectionClass self, GlibString string) => (self) -> ReactiveFieldRead IO (string)
fileSelectionGetFilenamePassive w = wrapMRPassive (fileSelectionGetFilename w)


-- @G: fileSelectionGetSelections					 | ["file","Selection","Get","Selections"]
fileSelectionGetSelectionsPassive :: (FileSelectionClass self, GlibString string) => (self) -> ReactiveFieldRead IO ([string])
fileSelectionGetSelectionsPassive w = wrapMRPassive (fileSelectionGetSelections w)


-- @G: fileSelectionGetSelectMultiple					 | ["file","Selection","Get","Select","Multiple"]
fileSelectionGetSelectMultiplePassive :: FileSelectionClass self => (self) -> ReactiveFieldRead IO (Bool)
fileSelectionGetSelectMultiplePassive w = wrapMRPassive (fileSelectionGetSelectMultiple w)


-- @A: fileSelectionSelectMultiple
fileSelectionSelectMultiplePassive :: FileSelectionClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
fileSelectionSelectMultiplePassive w = passivePropertyNE w fileSelectionSelectMultiple


-- @T: fileSelectionSetFilename					 | ["file","Selection","Set","Filename"]
fileSelectionSetFilenameReactive :: (FileSelectionClass self, GlibString string) => (self) -> ReactiveFieldWrite IO (string)
fileSelectionSetFilenameReactive w = wrapMW (fileSelectionSetFilename w)


-- @T: fileSelectionSetSelectMultiple					 | ["file","Selection","Set","Select","Multiple"]
fileSelectionSetSelectMultipleReactive :: FileSelectionClass self => (self) -> ReactiveFieldWrite IO (Bool)
fileSelectionSetSelectMultipleReactive w = wrapMW (fileSelectionSetSelectMultiple w)


-- @A: fileSelectionShowFileops
fileSelectionShowFileopsPassive :: FileSelectionClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
fileSelectionShowFileopsPassive w = passivePropertyNE w fileSelectionShowFileops


-- @C: afterFontSet
afterFontSetReactive :: FontButtonClass self => self -> ReactiveFieldRead IO ()
afterFontSetReactive w = reactivePropertyH_ w afterFontSet

-- @A: fontButtonFontName
fontButtonFontNamePassive :: (FontButtonClass self, GlibString string) => (self) -> ReactiveFieldReadWrite IO (string)
fontButtonFontNamePassive w = passivePropertyNE w fontButtonFontName


-- @G: fontButtonGetFontName					 | ["font","Button","Get","Font","Name"]
fontButtonGetFontNamePassive :: (FontButtonClass self, GlibString string) => (self) -> ReactiveFieldRead IO (string)
fontButtonGetFontNamePassive w = wrapMRPassive (fontButtonGetFontName w)


-- @G: fontButtonGetShowSize					 | ["font","Button","Get","Show","Size"]
fontButtonGetShowSizePassive :: FontButtonClass self => (self) -> ReactiveFieldRead IO (Bool)
fontButtonGetShowSizePassive w = wrapMRPassive (fontButtonGetShowSize w)


-- @G: fontButtonGetShowStyle					 | ["font","Button","Get","Show","Style"]
fontButtonGetShowStylePassive :: FontButtonClass self => (self) -> ReactiveFieldRead IO (Bool)
fontButtonGetShowStylePassive w = wrapMRPassive (fontButtonGetShowStyle w)


-- @G: fontButtonGetTitle					 | ["font","Button","Get","Title"]
fontButtonGetTitlePassive :: (FontButtonClass self, GlibString string) => (self) -> ReactiveFieldRead IO (string)
fontButtonGetTitlePassive w = wrapMRPassive (fontButtonGetTitle w)


-- @G: fontButtonGetUseFont					 | ["font","Button","Get","Use","Font"]
fontButtonGetUseFontPassive :: FontButtonClass self => (self) -> ReactiveFieldRead IO (Bool)
fontButtonGetUseFontPassive w = wrapMRPassive (fontButtonGetUseFont w)


-- @G: fontButtonGetUseSize					 | ["font","Button","Get","Use","Size"]
fontButtonGetUseSizePassive :: FontButtonClass self => (self) -> ReactiveFieldRead IO (Bool)
fontButtonGetUseSizePassive w = wrapMRPassive (fontButtonGetUseSize w)


-- @T: fontButtonSetFontName					 | ["font","Button","Set","Font","Name"]
fontButtonSetFontNameReactive :: (FontButtonClass self, GlibString string) => (self) -> ReactiveFieldWrite IO (string)
fontButtonSetFontNameReactive w = wrapMW (void . fontButtonSetFontName w)

-- @T: fontButtonSetShowSize					 | ["font","Button","Set","Show","Size"]
fontButtonSetShowSizeReactive :: FontButtonClass self => (self) -> ReactiveFieldWrite IO (Bool)
fontButtonSetShowSizeReactive w = wrapMW (fontButtonSetShowSize w)


-- @T: fontButtonSetShowStyle					 | ["font","Button","Set","Show","Style"]
fontButtonSetShowStyleReactive :: FontButtonClass self => (self) -> ReactiveFieldWrite IO (Bool)
fontButtonSetShowStyleReactive w = wrapMW (fontButtonSetShowStyle w)


-- @T: fontButtonSetTitle					 | ["font","Button","Set","Title"]
fontButtonSetTitleReactive :: (FontButtonClass self, GlibString string) => (self) -> ReactiveFieldWrite IO (string)
fontButtonSetTitleReactive w = wrapMW (fontButtonSetTitle w)


-- @T: fontButtonSetUseFont					 | ["font","Button","Set","Use","Font"]
fontButtonSetUseFontReactive :: FontButtonClass self => (self) -> ReactiveFieldWrite IO (Bool)
fontButtonSetUseFontReactive w = wrapMW (fontButtonSetUseFont w)


-- @T: fontButtonSetUseSize					 | ["font","Button","Set","Use","Size"]
fontButtonSetUseSizeReactive :: FontButtonClass self => (self) -> ReactiveFieldWrite IO (Bool)
fontButtonSetUseSizeReactive w = wrapMW (fontButtonSetUseSize w)


-- @A: fontButtonShowSize
fontButtonShowSizePassive :: FontButtonClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
fontButtonShowSizePassive w = passivePropertyNE w fontButtonShowSize


-- @A: fontButtonShowStyle
fontButtonShowStylePassive :: FontButtonClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
fontButtonShowStylePassive w = passivePropertyNE w fontButtonShowStyle


-- @A: fontButtonTitle
fontButtonTitlePassive :: (FontButtonClass self, GlibString string) => (self) -> ReactiveFieldReadWrite IO (string)
fontButtonTitlePassive w = passivePropertyNE w fontButtonTitle


-- @A: fontButtonUseFont
fontButtonUseFontPassive :: FontButtonClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
fontButtonUseFontPassive w = passivePropertyNE w fontButtonUseFont


-- @A: fontButtonUseSize
fontButtonUseSizePassive :: FontButtonClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
fontButtonUseSizePassive w = passivePropertyNE w fontButtonUseSize


-- @C: onFontSet
onFontSetReactive :: FontButtonClass self => self -> ReactiveFieldRead IO ()
onFontSetReactive w = reactivePropertyH_ w onFontSet

-- @G: fontSelectionDialogGetCancelButton					 | ["font","Selection","Dialog","Get","Cancel","Button"]
fontSelectionDialogGetCancelButtonPassive :: FontSelectionDialogClass self => (self) -> ReactiveFieldRead IO (Widget)
fontSelectionDialogGetCancelButtonPassive w = wrapMRPassive (fontSelectionDialogGetCancelButton w)


-- @G: fontSelectionDialogGetFontName					 | ["font","Selection","Dialog","Get","Font","Name"]
fontSelectionDialogGetFontNamePassive :: (FontSelectionDialogClass self, GlibString string) => (self) -> ReactiveFieldRead IO ((Maybe string))
fontSelectionDialogGetFontNamePassive w = wrapMRPassive (fontSelectionDialogGetFontName w)


-- @G: fontSelectionDialogGetFontSelection					 | ["font","Selection","Dialog","Get","Font","Selection"]
fontSelectionDialogGetFontSelectionPassive :: FontSelectionDialogClass self => (self) -> ReactiveFieldRead IO (FontSelection)
fontSelectionDialogGetFontSelectionPassive w = wrapMRPassive (fontSelectionDialogGetFontSelection w)


-- @G: fontSelectionDialogGetOkButton					 | ["font","Selection","Dialog","Get","Ok","Button"]
fontSelectionDialogGetOkButtonPassive :: FontSelectionDialogClass self => (self) -> ReactiveFieldRead IO (Widget)
fontSelectionDialogGetOkButtonPassive w = wrapMRPassive (fontSelectionDialogGetOkButton w)


-- @G: fontSelectionDialogGetPreviewText					 | ["font","Selection","Dialog","Get","Preview","Text"]
fontSelectionDialogGetPreviewTextPassive :: (FontSelectionDialogClass self, GlibString string) => (self) -> ReactiveFieldRead IO (string)
fontSelectionDialogGetPreviewTextPassive w = wrapMRPassive (fontSelectionDialogGetPreviewText w)


-- @A: fontSelectionDialogPreviewText
fontSelectionDialogPreviewTextPassive :: (FontSelectionDialogClass self, GlibString string) => (self) -> ReactiveFieldReadWrite IO (string)
fontSelectionDialogPreviewTextPassive w = passivePropertyNE w fontSelectionDialogPreviewText


-- @T: fontSelectionDialogSetFontName					 | ["font","Selection","Dialog","Set","Font","Name"]
fontSelectionDialogSetFontNameReactive :: (FontSelectionDialogClass self, GlibString string) => (self) -> ReactiveFieldWrite IO (string)
fontSelectionDialogSetFontNameReactive w = wrapMW (void . fontSelectionDialogSetFontName w)

-- @T: fontSelectionDialogSetPreviewText					 | ["font","Selection","Dialog","Set","Preview","Text"]
fontSelectionDialogSetPreviewTextReactive :: (FontSelectionDialogClass self, GlibString string) => (self) -> ReactiveFieldWrite IO (string)
fontSelectionDialogSetPreviewTextReactive w = wrapMW (fontSelectionDialogSetPreviewText w)


-- @A: fontSelectionFontName
fontSelectionFontNamePassive :: (FontSelectionClass self, GlibString string) => (self) -> ReactiveFieldReadWrite IO (string)
fontSelectionFontNamePassive w = passivePropertyNE w fontSelectionFontName


-- @G: fontSelectionGetFontName					 | ["font","Selection","Get","Font","Name"]
fontSelectionGetFontNamePassive :: (FontSelectionClass self, GlibString string) => (self) -> ReactiveFieldRead IO ((Maybe string))
fontSelectionGetFontNamePassive w = wrapMRPassive (fontSelectionGetFontName w)


-- @G: fontSelectionGetPreviewText					 | ["font","Selection","Get","Preview","Text"]
fontSelectionGetPreviewTextPassive :: (FontSelectionClass self, GlibString string) => (self) -> ReactiveFieldRead IO (string)
fontSelectionGetPreviewTextPassive w = wrapMRPassive (fontSelectionGetPreviewText w)


-- @A: fontSelectionPreviewText
fontSelectionPreviewTextPassive :: (FontSelectionClass self, GlibString string) => (self) -> ReactiveFieldReadWrite IO (string)
fontSelectionPreviewTextPassive w = passivePropertyNE w fontSelectionPreviewText


-- @T: fontSelectionSetFontName					 | ["font","Selection","Set","Font","Name"]
fontSelectionSetFontNameReactive :: (FontSelectionClass self, GlibString string) => (self) -> ReactiveFieldWrite IO (string)
fontSelectionSetFontNameReactive w = wrapMW (void . fontSelectionSetFontName w)

-- @T: fontSelectionSetPreviewText					 | ["font","Selection","Set","Preview","Text"]
fontSelectionSetPreviewTextReactive :: (FontSelectionClass self, GlibString string) => (self) -> ReactiveFieldWrite IO (string)
fontSelectionSetPreviewTextReactive w = wrapMW (fontSelectionSetPreviewText w)


-- @S: hsvChanged
hsvChangedReactive :: HSVClass self => self -> ReactiveFieldRead IO ()
hsvChangedReactive = (`reactiveSignalIO` hsvChanged)


-- @A: hsvColor
hsvColorPassive :: HSVClass self => (self) -> ReactiveFieldReadWrite IO ((Double, Double, Double))
hsvColorPassive w = passivePropertyNE w hsvColor


-- @A: hsvMetrics
hsvMetricsPassive :: HSVClass self => (self) -> ReactiveFieldReadWrite IO ((Int, Int))
hsvMetricsPassive w = passivePropertyNE w hsvMetrics


-- @S: hsvMove
-- TODO
-- @A: rulerLower
rulerLowerPassive :: RulerClass self => (self) -> ReactiveFieldReadWrite IO (Double)
rulerLowerPassive w = passivePropertyNE w rulerLower


-- @A: rulerMaxSize
rulerMaxSizePassive :: RulerClass self => (self) -> ReactiveFieldReadWrite IO (Double)
rulerMaxSizePassive w = passivePropertyNE w rulerMaxSize


-- @A: rulerMetric
rulerMetricPassive :: RulerClass self => (self) -> ReactiveFieldReadWrite IO (MetricType)
rulerMetricPassive w = passivePropertyNE w rulerMetric


-- @A: rulerPosition
rulerPositionPassive :: RulerClass self => (self) -> ReactiveFieldReadWrite IO (Double)
rulerPositionPassive w = passivePropertyNE w rulerPosition


-- @A: rulerRange
rulerRangePassive :: RulerClass self => (self) -> ReactiveFieldReadWrite IO ((Double, Double, Double, Double))
rulerRangePassive w = passivePropertyNE w rulerRange


-- @A: rulerUpper
rulerUpperPassive :: RulerClass self => (self) -> ReactiveFieldReadWrite IO (Double)
rulerUpperPassive w = passivePropertyNE w rulerUpper


-- @A: aboutDialogArtists
aboutDialogArtistsPassive :: (AboutDialogClass self, GlibString string) => (self) -> ReactiveFieldReadWrite IO ([string])
aboutDialogArtistsPassive w = passivePropertyNE w aboutDialogArtists


-- @A: aboutDialogAuthors
aboutDialogAuthorsPassive :: (AboutDialogClass self, GlibString string) => (self) -> ReactiveFieldReadWrite IO ([string])
aboutDialogAuthorsPassive w = passivePropertyNE w aboutDialogAuthors


-- @A: aboutDialogComments
aboutDialogCommentsPassive :: (AboutDialogClass self, GlibString string) => (self) -> ReactiveFieldReadWrite IO (string)
aboutDialogCommentsPassive w = passivePropertyNE w aboutDialogComments


-- @A: aboutDialogCopyright
aboutDialogCopyrightPassive :: (AboutDialogClass self, GlibString string) => (self) -> ReactiveFieldReadWrite IO (string)
aboutDialogCopyrightPassive w = passivePropertyNE w aboutDialogCopyright


-- @A: aboutDialogDocumenters
aboutDialogDocumentersPassive :: (AboutDialogClass self, GlibString string) => (self) -> ReactiveFieldReadWrite IO ([string])
aboutDialogDocumentersPassive w = passivePropertyNE w aboutDialogDocumenters


-- @G: aboutDialogGetArtists					 | ["about","Dialog","Get","Artists"]
aboutDialogGetArtistsPassive :: (AboutDialogClass self, GlibString string) => (self) -> ReactiveFieldRead IO ([string])
aboutDialogGetArtistsPassive w = wrapMRPassive (aboutDialogGetArtists w)


-- @G: aboutDialogGetAuthors					 | ["about","Dialog","Get","Authors"]
aboutDialogGetAuthorsPassive :: (AboutDialogClass self, GlibString string) => (self) -> ReactiveFieldRead IO ([string])
aboutDialogGetAuthorsPassive w = wrapMRPassive (aboutDialogGetAuthors w)


-- @G: aboutDialogGetComments					 | ["about","Dialog","Get","Comments"]
aboutDialogGetCommentsPassive :: (AboutDialogClass self, GlibString string) => (self) -> ReactiveFieldRead IO (string)
aboutDialogGetCommentsPassive w = wrapMRPassive (aboutDialogGetComments w)


-- @G: aboutDialogGetCopyright					 | ["about","Dialog","Get","Copyright"]
aboutDialogGetCopyrightPassive :: (AboutDialogClass self, GlibString string) => (self) -> ReactiveFieldRead IO (string)
aboutDialogGetCopyrightPassive w = wrapMRPassive (aboutDialogGetCopyright w)


-- @G: aboutDialogGetDocumenters					 | ["about","Dialog","Get","Documenters"]
aboutDialogGetDocumentersPassive :: (AboutDialogClass self, GlibString string) => (self) -> ReactiveFieldRead IO ([string])
aboutDialogGetDocumentersPassive w = wrapMRPassive (aboutDialogGetDocumenters w)


-- @G: aboutDialogGetLicense					 | ["about","Dialog","Get","License"]
aboutDialogGetLicensePassive :: (AboutDialogClass self, GlibString string) => (self) -> ReactiveFieldRead IO ((Maybe string))
aboutDialogGetLicensePassive w = wrapMRPassive (aboutDialogGetLicense w)


-- @G: aboutDialogGetLogo					 | ["about","Dialog","Get","Logo"]
aboutDialogGetLogoPassive :: AboutDialogClass self => (self) -> ReactiveFieldRead IO (Pixbuf)
aboutDialogGetLogoPassive w = wrapMRPassive (aboutDialogGetLogo w)


-- @G: aboutDialogGetLogoIconName					 | ["about","Dialog","Get","Logo","Icon","Name"]
aboutDialogGetLogoIconNamePassive :: (AboutDialogClass self, GlibString string) => (self) -> ReactiveFieldRead IO (string)
aboutDialogGetLogoIconNamePassive w = wrapMRPassive (aboutDialogGetLogoIconName w)


-- @G: aboutDialogGetName					 | ["about","Dialog","Get","Name"]
aboutDialogGetNamePassive :: (AboutDialogClass self, GlibString string) => (self) -> ReactiveFieldRead IO (string)
aboutDialogGetNamePassive w = wrapMRPassive (aboutDialogGetName w)


-- @G: aboutDialogGetTranslatorCredits					 | ["about","Dialog","Get","Translator","Credits"]
aboutDialogGetTranslatorCreditsPassive :: (AboutDialogClass self, GlibString string) => (self) -> ReactiveFieldRead IO (string)
aboutDialogGetTranslatorCreditsPassive w = wrapMRPassive (aboutDialogGetTranslatorCredits w)


-- @G: aboutDialogGetVersion					 | ["about","Dialog","Get","Version"]
aboutDialogGetVersionPassive :: (AboutDialogClass self, GlibString string) => (self) -> ReactiveFieldRead IO (string)
aboutDialogGetVersionPassive w = wrapMRPassive (aboutDialogGetVersion w)


-- @G: aboutDialogGetWebsite					 | ["about","Dialog","Get","Website"]
aboutDialogGetWebsitePassive :: (AboutDialogClass self, GlibString string) => (self) -> ReactiveFieldRead IO (string)
aboutDialogGetWebsitePassive w = wrapMRPassive (aboutDialogGetWebsite w)


-- @G: aboutDialogGetWebsiteLabel					 | ["about","Dialog","Get","Website","Label"]
aboutDialogGetWebsiteLabelPassive :: (AboutDialogClass self, GlibString string) => (self) -> ReactiveFieldRead IO (string)
aboutDialogGetWebsiteLabelPassive w = wrapMRPassive (aboutDialogGetWebsiteLabel w)


-- @G: aboutDialogGetWrapLicense					 | ["about","Dialog","Get","Wrap","License"]
aboutDialogGetWrapLicensePassive :: AboutDialogClass self => (self) -> ReactiveFieldRead IO (Bool)
aboutDialogGetWrapLicensePassive w = wrapMRPassive (aboutDialogGetWrapLicense w)


-- @A: aboutDialogLicense
aboutDialogLicensePassive :: (AboutDialogClass self, GlibString string) => (self) -> ReactiveFieldReadWrite IO ((Maybe string))
aboutDialogLicensePassive w = passivePropertyNE w aboutDialogLicense


-- @A: aboutDialogName
aboutDialogNamePassive :: (AboutDialogClass self, GlibString string) => (self) -> ReactiveFieldReadWrite IO (string)
aboutDialogNamePassive w = passivePropertyNE w aboutDialogName


-- @A: aboutDialogProgramName
aboutDialogProgramNamePassive :: (AboutDialogClass self, GlibString string) => (self) -> ReactiveFieldReadWrite IO (string)
aboutDialogProgramNamePassive w = passivePropertyNE w aboutDialogProgramName


-- @T: aboutDialogSetArtists					 | ["about","Dialog","Set","Artists"]
aboutDialogSetArtistsReactive :: (AboutDialogClass self, GlibString string) => (self) -> ReactiveFieldWrite IO ([string])
aboutDialogSetArtistsReactive w = wrapMW (aboutDialogSetArtists w)


-- @T: aboutDialogSetAuthors					 | ["about","Dialog","Set","Authors"]
aboutDialogSetAuthorsReactive :: (AboutDialogClass self, GlibString string) => (self) -> ReactiveFieldWrite IO ([string])
aboutDialogSetAuthorsReactive w = wrapMW (aboutDialogSetAuthors w)


-- @T: aboutDialogSetComments					 | ["about","Dialog","Set","Comments"]
aboutDialogSetCommentsReactive :: (AboutDialogClass self, GlibString string) => (self) -> ReactiveFieldWrite IO (string)
aboutDialogSetCommentsReactive w = wrapMW (aboutDialogSetComments w)


-- @T: aboutDialogSetCopyright					 | ["about","Dialog","Set","Copyright"]
aboutDialogSetCopyrightReactive :: (AboutDialogClass self, GlibString string) => (self) -> ReactiveFieldWrite IO (string)
aboutDialogSetCopyrightReactive w = wrapMW (aboutDialogSetCopyright w)


-- @T: aboutDialogSetDocumenters					 | ["about","Dialog","Set","Documenters"]
aboutDialogSetDocumentersReactive :: (AboutDialogClass self, GlibString string) => (self) -> ReactiveFieldWrite IO ([string])
aboutDialogSetDocumentersReactive w = wrapMW (aboutDialogSetDocumenters w)


-- @T: aboutDialogSetEmailHook					 | ["about","Dialog","Set","Email","Hook"]
-- TODO
-- @T: aboutDialogSetLicense					 | ["about","Dialog","Set","License"]
aboutDialogSetLicenseReactive :: (AboutDialogClass self, GlibString string) => (self) -> ReactiveFieldWrite IO (Maybe string)
aboutDialogSetLicenseReactive w = wrapMW (aboutDialogSetLicense w)


-- @T: aboutDialogSetLogo					 | ["about","Dialog","Set","Logo"]
aboutDialogSetLogoReactive :: AboutDialogClass self => (self) -> ReactiveFieldWrite IO (Maybe Pixbuf)
aboutDialogSetLogoReactive w = wrapMW (aboutDialogSetLogo w)


-- @T: aboutDialogSetLogoIconName					 | ["about","Dialog","Set","Logo","Icon","Name"]
aboutDialogSetLogoIconNameReactive :: (AboutDialogClass self, GlibString string) => (self) -> ReactiveFieldWrite IO (Maybe string)
aboutDialogSetLogoIconNameReactive w = wrapMW (aboutDialogSetLogoIconName w)


-- @T: aboutDialogSetName					 | ["about","Dialog","Set","Name"]
aboutDialogSetNameReactive :: (AboutDialogClass self, GlibString string) => (self) -> ReactiveFieldWrite IO (string)
aboutDialogSetNameReactive w = wrapMW (aboutDialogSetName w)


-- @T: aboutDialogSetTranslatorCredits					 | ["about","Dialog","Set","Translator","Credits"]
aboutDialogSetTranslatorCreditsReactive :: (AboutDialogClass self, GlibString string) => (self) -> ReactiveFieldWrite IO (string)
aboutDialogSetTranslatorCreditsReactive w = wrapMW (aboutDialogSetTranslatorCredits w)


-- @T: aboutDialogSetUrlHook					 | ["about","Dialog","Set","Url","Hook"]
-- TODO
-- @T: aboutDialogSetVersion					 | ["about","Dialog","Set","Version"]
aboutDialogSetVersionReactive :: (AboutDialogClass self, GlibString string) => (self) -> ReactiveFieldWrite IO (string)
aboutDialogSetVersionReactive w = wrapMW (aboutDialogSetVersion w)


-- @T: aboutDialogSetWebsite					 | ["about","Dialog","Set","Website"]
aboutDialogSetWebsiteReactive :: (AboutDialogClass self, GlibString string) => (self) -> ReactiveFieldWrite IO (string)
aboutDialogSetWebsiteReactive w = wrapMW (aboutDialogSetWebsite w)


-- @T: aboutDialogSetWebsiteLabel					 | ["about","Dialog","Set","Website","Label"]
aboutDialogSetWebsiteLabelReactive :: (AboutDialogClass self, GlibString string) => (self) -> ReactiveFieldWrite IO (string)
aboutDialogSetWebsiteLabelReactive w = wrapMW (aboutDialogSetWebsiteLabel w)


-- @T: aboutDialogSetWrapLicense					 | ["about","Dialog","Set","Wrap","License"]
aboutDialogSetWrapLicenseReactive :: AboutDialogClass self => (self) -> ReactiveFieldWrite IO (Bool)
aboutDialogSetWrapLicenseReactive w = wrapMW (aboutDialogSetWrapLicense w)


-- @A: aboutDialogTranslatorCredits
aboutDialogTranslatorCreditsPassive :: (AboutDialogClass self, GlibString string) => (self) -> ReactiveFieldReadWrite IO (string)
aboutDialogTranslatorCreditsPassive w = passivePropertyNE w aboutDialogTranslatorCredits


-- @A: aboutDialogVersion
aboutDialogVersionPassive :: (AboutDialogClass self, GlibString string) => (self) -> ReactiveFieldReadWrite IO (string)
aboutDialogVersionPassive w = passivePropertyNE w aboutDialogVersion


-- @A: aboutDialogWebsite
aboutDialogWebsitePassive :: (AboutDialogClass self, GlibString string) => (self) -> ReactiveFieldReadWrite IO (string)
aboutDialogWebsitePassive w = passivePropertyNE w aboutDialogWebsite


-- @A: aboutDialogWebsiteLabel
aboutDialogWebsiteLabelPassive :: (AboutDialogClass self, GlibString string) => (self) -> ReactiveFieldReadWrite IO (string)
aboutDialogWebsiteLabelPassive w = passivePropertyNE w aboutDialogWebsiteLabel


-- @A: aboutDialogWrapLicense
aboutDialogWrapLicensePassive :: AboutDialogClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
aboutDialogWrapLicensePassive w = passivePropertyNE w aboutDialogWrapLicense


-- @S: assistantApply
assistantApplyReactive :: AssistantClass self => self -> ReactiveFieldRead IO ()
assistantApplyReactive = (`reactiveSignalIO` assistantApply)


-- @S: assistantCancel
assistantCancelReactive :: AssistantClass self => self -> ReactiveFieldRead IO ()
assistantCancelReactive = (`reactiveSignalIO` assistantCancel)


-- @A: assistantChildComplete
assistantChildCompletePassive :: AssistantClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
assistantChildCompletePassive w = passivePropertyNE w assistantChildComplete


-- @A: assistantChildHeaderImage
assistantChildHeaderImagePassive :: AssistantClass self => (self) -> ReactiveFieldReadWrite IO (Pixbuf)
assistantChildHeaderImagePassive w = passivePropertyNE w assistantChildHeaderImage


-- @A: assistantChildPageType
assistantChildPageTypePassive :: AssistantClass self => (self) -> ReactiveFieldReadWrite IO (AssistantPageType)
assistantChildPageTypePassive w = passivePropertyNE w assistantChildPageType


-- @A: assistantChildSidebarImage
assistantChildSidebarImagePassive :: AssistantClass self => (self) -> ReactiveFieldReadWrite IO (Pixbuf)
assistantChildSidebarImagePassive w = passivePropertyNE w assistantChildSidebarImage


-- @A: assistantChildTitle
assistantChildTitlePassive :: (AssistantClass self, GlibString string) => (self) -> ReactiveFieldReadWrite IO (string)
assistantChildTitlePassive w = passivePropertyNE w assistantChildTitle


-- @S: assistantClose
assistantCloseReactive :: AssistantClass self => self -> ReactiveFieldRead IO ()
assistantCloseReactive = (`reactiveSignalIO` assistantClose)


-- @A: assistantCurrentPage
assistantCurrentPagePassive :: AssistantClass self => (self) -> ReactiveFieldReadWrite IO (Int)
assistantCurrentPagePassive w = passivePropertyNE w assistantCurrentPage


-- @G: assistantGetNPages					 | ["assistant","Get","NPages"]
assistantGetNPagesPassive :: AssistantClass self => (self) -> ReactiveFieldRead IO (Int)
assistantGetNPagesPassive w = wrapMRPassive (assistantGetNPages w)


-- @G: assistantGetNthPage					 | ["assistant","Get","Nth","Page"]
-- TODO
-- @G: assistantGetPageComplete					 | ["assistant","Get","Page","Complete"]
-- TODO
-- @G: assistantGetPageHeaderImage					 | ["assistant","Get","Page","Header","Image"]
-- TODO
-- @G: assistantGetPageSideImage					 | ["assistant","Get","Page","Side","Image"]
-- TODO
-- @G: assistantGetPageTitle					 | ["assistant","Get","Page","Title"]
-- TODO
-- @G: assistantGetPageType					 | ["assistant","Get","Page","Type"]
-- TODO
-- @S: assistantPrepare
-- TODO
-- @T: assistantSetForwardPageFunc					 | ["assistant","Set","Forward","Page","Func"]
assistantSetForwardPageFuncReactive :: AssistantClass self => (self) -> ReactiveFieldWrite IO (Maybe (Int -> IO Int))
assistantSetForwardPageFuncReactive w = wrapMW (assistantSetForwardPageFunc w)


-- @T: assistantSetPageComplete					 | ["assistant","Set","Page","Complete"]
-- TODO
-- @T: assistantSetPageHeaderImage					 | ["assistant","Set","Page","Header","Image"]
-- TODO
-- @T: assistantSetPageSideImage					 | ["assistant","Set","Page","Side","Image"]
-- TODO
-- @T: assistantSetPageTitle					 | ["assistant","Set","Page","Title"]
-- TODO
-- @T: assistantSetPageType					 | ["assistant","Set","Page","Type"]
-- TODO
-- @C: afterResponse
-- TODO
-- @G: dialogGetContentArea					 | ["dialog","Get","Content","Area"]
dialogGetContentAreaPassive :: DialogClass dc => (dc) -> ReactiveFieldRead IO (Widget)
dialogGetContentAreaPassive w = wrapMRPassive (dialogGetContentArea w)


-- @G: dialogGetHasSeparator					 | ["dialog","Get","Has","Separator"]
dialogGetHasSeparatorPassive :: DialogClass self => (self) -> ReactiveFieldRead IO (Bool)
dialogGetHasSeparatorPassive w = wrapMRPassive (dialogGetHasSeparator w)


-- @G: dialogGetResponseForWidget					 | ["dialog","Get","Response","For","Widget"]
-- TODO
-- @G: dialogGetWidgetForResponse					 | ["dialog","Get","Widget","For","Response"]
-- TODO
-- @A: dialogHasSeparator
dialogHasSeparatorPassive :: DialogClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
dialogHasSeparatorPassive w = passivePropertyNE w dialogHasSeparator


-- @T: dialogSetAlternativeButtonOrderFromArray					 | ["dialog","Set","Alternative","Button","Order","From","Array"]
dialogSetAlternativeButtonOrderFromArrayReactive :: DialogClass self => (self) -> ReactiveFieldWrite IO ([ResponseId])
dialogSetAlternativeButtonOrderFromArrayReactive w = wrapMW (dialogSetAlternativeButtonOrderFromArray w)


-- @T: dialogSetDefaultResponse					 | ["dialog","Set","Default","Response"]
dialogSetDefaultResponseReactive :: DialogClass self => (self) -> ReactiveFieldWrite IO (ResponseId)
dialogSetDefaultResponseReactive w = wrapMW (dialogSetDefaultResponse w)


-- @T: dialogSetHasSeparator					 | ["dialog","Set","Has","Separator"]
dialogSetHasSeparatorReactive :: DialogClass self => (self) -> ReactiveFieldWrite IO (Bool)
dialogSetHasSeparatorReactive w = wrapMW (dialogSetHasSeparator w)


-- @T: dialogSetResponseSensitive					 | ["dialog","Set","Response","Sensitive"]
-- TODO
-- @C: onResponse
-- TODO
-- @S: response
-- TODO
-- @G: invisibleGetScreen					 | ["invisible","Get","Screen"]
invisibleGetScreenPassive :: (Invisible) -> ReactiveFieldRead IO (Screen)
invisibleGetScreenPassive w = wrapMRPassive (invisibleGetScreen w)


-- @T: invisibleSetScreen					 | ["invisible","Set","Screen"]
invisibleSetScreenPassive :: (Invisible) -> ReactiveFieldWrite IO (Screen)
invisibleSetScreenPassive w = wrapMW (invisibleSetScreen w)

-- @A: messageDialogMessageType
messageDialogMessageTypePassive :: MessageDialogClass self => (self) -> ReactiveFieldReadWrite IO (MessageType)
messageDialogMessageTypePassive w = passivePropertyNE w messageDialogMessageType


-- @A: messageDialogSecondaryText
messageDialogSecondaryTextPassive :: (MessageDialogClass self, GlibString string) => (self) -> ReactiveFieldReadWrite IO ((Maybe string))
messageDialogSecondaryTextPassive w = passivePropertyNE w messageDialogSecondaryText


-- @A: messageDialogSecondaryUseMarkup
messageDialogSecondaryUseMarkupPassive :: MessageDialogClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
messageDialogSecondaryUseMarkupPassive w = passivePropertyNE w messageDialogSecondaryUseMarkup


-- @T: messageDialogSetImage					 | ["message","Dialog","Set","Image"]
messageDialogSetImageReactive :: (MessageDialogClass self, WidgetClass image) => (self) -> ReactiveFieldWrite IO (image)
messageDialogSetImageReactive w = wrapMW (messageDialogSetImage w)


-- @T: messageDialogSetMarkup					 | ["message","Dialog","Set","Markup"]
messageDialogSetMarkupReactive :: (MessageDialogClass self, GlibString string) => (self) -> ReactiveFieldWrite IO (string)
messageDialogSetMarkupReactive w = wrapMW (messageDialogSetMarkup w)


-- @T: messageDialogSetSecondaryMarkup					 | ["message","Dialog","Set","Secondary","Markup"]
messageDialogSetSecondaryMarkupReactive :: (MessageDialogClass self, GlibString string) => (self) -> ReactiveFieldWrite IO (string)
messageDialogSetSecondaryMarkupReactive w = wrapMW (messageDialogSetSecondaryMarkup w)


-- @T: messageDialogSetSecondaryText					 | ["message","Dialog","Set","Secondary","Text"]
messageDialogSetSecondaryTextReactive :: (MessageDialogClass self, GlibString string) => (self) -> ReactiveFieldWrite IO (string)
messageDialogSetSecondaryTextReactive w = wrapMW (messageDialogSetSecondaryText w)


-- @A: messageDialogText
messageDialogTextPassive :: (MessageDialogClass self, GlibString string) => (self) -> ReactiveFieldReadWrite IO ((Maybe string))
messageDialogTextPassive w = passivePropertyNE w messageDialogText


-- @A: messageDialogUseMarkup
messageDialogUseMarkupPassive :: MessageDialogClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
messageDialogUseMarkupPassive w = passivePropertyNE w messageDialogUseMarkup


-- @G: offscreenWindowGetPixbuf					 | ["offscreen","Window","Get","Pixbuf"]
offscreenWindowGetPixbufPassive :: OffscreenWindowClass self => (self) -> ReactiveFieldRead IO ((Maybe Pixbuf))
offscreenWindowGetPixbufPassive w = wrapMRPassive (offscreenWindowGetPixbuf w)


-- @G: offscreenWindowGetPixmap					 | ["offscreen","Window","Get","Pixmap"]
offscreenWindowGetPixmapPassive :: OffscreenWindowClass self => (self) -> ReactiveFieldRead IO ((Maybe Pixmap))
offscreenWindowGetPixmapPassive w = wrapMRPassive (offscreenWindowGetPixmap w)


-- @C: afterSetFocus
-- TODO
-- @S: frameEvent
-- TODO
-- @S: keysChanged
keysChangedReactive :: WindowClass self => self -> ReactiveFieldRead IO ()
keysChangedReactive = (`reactiveSignalIO` keysChanged)


-- @C: onSetFocus
-- TODO
-- @S: setFocus
-- TODO
-- @A: windowAcceptFocus
windowAcceptFocusPassive :: WindowClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
windowAcceptFocusPassive w = passivePropertyNE w windowAcceptFocus


-- @A: windowAllowGrow
windowAllowGrowPassive :: WindowClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
windowAllowGrowPassive w = passivePropertyNE w windowAllowGrow


-- @A: windowAllowShrink
windowAllowShrinkPassive :: WindowClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
windowAllowShrinkPassive w = passivePropertyNE w windowAllowShrink


-- @A: windowDecorated
windowDecoratedPassive :: WindowClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
windowDecoratedPassive w = passivePropertyNE w windowDecorated


-- @A: windowDefaultHeight
windowDefaultHeightPassive :: WindowClass self => (self) -> ReactiveFieldReadWrite IO (Int)
windowDefaultHeightPassive w = passivePropertyNE w windowDefaultHeight


-- @A: windowDefaultWidth
windowDefaultWidthPassive :: WindowClass self => (self) -> ReactiveFieldReadWrite IO (Int)
windowDefaultWidthPassive w = passivePropertyNE w windowDefaultWidth


-- @A: windowDeletable
windowDeletablePassive :: WindowClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
windowDeletablePassive w = passivePropertyNE w windowDeletable


-- @A: windowDestroyWithParent
windowDestroyWithParentPassive :: WindowClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
windowDestroyWithParentPassive w = passivePropertyNE w windowDestroyWithParent


-- @A: windowFocus
windowFocusPassive :: WindowClass self => (self) -> ReactiveFieldReadWrite IO ((Maybe Widget))
windowFocusPassive w = passivePropertyNE w windowFocus


-- @A: windowFocusOnMap
windowFocusOnMapPassive :: WindowClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
windowFocusOnMapPassive w = passivePropertyNE w windowFocusOnMap


-- @G: windowGetAcceptFocus					 | ["window","Get","Accept","Focus"]
windowGetAcceptFocusPassive :: WindowClass self => (self) -> ReactiveFieldRead IO (Bool)
windowGetAcceptFocusPassive w = wrapMRPassive (windowGetAcceptFocus w)


-- @G: windowGetDecorated					 | ["window","Get","Decorated"]
windowGetDecoratedPassive :: WindowClass self => (self) -> ReactiveFieldRead IO (Bool)
windowGetDecoratedPassive w = wrapMRPassive (windowGetDecorated w)


-- @G: windowGetDefaultIconList					 | ["window","Get","Default","Icon","List"]
windowGetDefaultIconListPassive :: ReactiveFieldRead IO ([Pixbuf])
windowGetDefaultIconListPassive = wrapMRPassive (windowGetDefaultIconList)


-- @G: windowGetDefaultIconName					 | ["window","Get","Default","Icon","Name"]
-- TODO
-- @G: windowGetDefaultSize					 | ["window","Get","Default","Size"]
windowGetDefaultSizePassive :: WindowClass self => (self) -> ReactiveFieldRead IO ((Int, Int))
windowGetDefaultSizePassive w = wrapMRPassive (windowGetDefaultSize w)


-- @G: windowGetDefaultWidget					 | ["window","Get","Default","Widget"]
windowGetDefaultWidgetPassive :: WindowClass self => (self) -> ReactiveFieldRead IO ((Maybe Widget))
windowGetDefaultWidgetPassive w = wrapMRPassive (windowGetDefaultWidget w)


-- @G: windowGetDeletable					 | ["window","Get","Deletable"]
windowGetDeletablePassive :: WindowClass self => (self) -> ReactiveFieldRead IO (Bool)
windowGetDeletablePassive w = wrapMRPassive (windowGetDeletable w)


-- @G: windowGetDestroyWithParent					 | ["window","Get","Destroy","With","Parent"]
windowGetDestroyWithParentPassive :: WindowClass self => (self) -> ReactiveFieldRead IO (Bool)
windowGetDestroyWithParentPassive w = wrapMRPassive (windowGetDestroyWithParent w)


-- @G: windowGetFocus					 | ["window","Get","Focus"]
windowGetFocusPassive :: WindowClass self => (self) -> ReactiveFieldRead IO ((Maybe Widget))
windowGetFocusPassive w = wrapMRPassive (windowGetFocus w)


-- @G: windowGetFocusOnMap					 | ["window","Get","Focus","On","Map"]
windowGetFocusOnMapPassive :: WindowClass self => (self) -> ReactiveFieldRead IO (Bool)
windowGetFocusOnMapPassive w = wrapMRPassive (windowGetFocusOnMap w)


-- @G: windowGetFrameDimensions					 | ["window","Get","Frame","Dimensions"]
windowGetFrameDimensionsPassive :: WindowClass self => (self) -> ReactiveFieldRead IO ((Int, Int, Int, Int))
windowGetFrameDimensionsPassive w = wrapMRPassive (windowGetFrameDimensions w)


-- @G: windowGetGravity					 | ["window","Get","Gravity"]
windowGetGravityPassive :: WindowClass self => (self) -> ReactiveFieldRead IO (Gravity)
windowGetGravityPassive w = wrapMRPassive (windowGetGravity w)


-- @G: windowGetGroup					 | ["window","Get","Group"]
windowGetGroupPassive :: WindowClass self => (Maybe self) -> ReactiveFieldRead IO (WindowGroup)
windowGetGroupPassive w = wrapMRPassive (windowGetGroup w)


-- @G: windowGetHasFrame					 | ["window","Get","Has","Frame"]
windowGetHasFramePassive :: WindowClass self => (self) -> ReactiveFieldRead IO (Bool)
windowGetHasFramePassive w = wrapMRPassive (windowGetHasFrame w)


-- @G: windowGetIcon					 | ["window","Get","Icon"]
windowGetIconPassive :: WindowClass self => (self) -> ReactiveFieldRead IO ((Maybe Pixbuf))
windowGetIconPassive w = wrapMRPassive (windowGetIcon w)


-- @G: windowGetIconList					 | ["window","Get","Icon","List"]
windowGetIconListPassive :: WindowClass self => (self) -> ReactiveFieldRead IO ([Pixbuf])
windowGetIconListPassive w = wrapMRPassive (windowGetIconList w)


-- @G: windowGetIconName					 | ["window","Get","Icon","Name"]
windowGetIconNamePassive :: (WindowClass self, GlibString string) => (self) -> ReactiveFieldRead IO (string)
windowGetIconNamePassive w = wrapMRPassive (windowGetIconName w)


-- @G: windowGetMnemonicModifier					 | ["window","Get","Mnemonic","Modifier"]
windowGetMnemonicModifierPassive :: WindowClass self => (self) -> ReactiveFieldRead IO ([Modifier])
windowGetMnemonicModifierPassive w = wrapMRPassive (windowGetMnemonicModifier w)


-- @G: windowGetModal					 | ["window","Get","Modal"]
windowGetModalPassive :: WindowClass self => (self) -> ReactiveFieldRead IO (Bool)
windowGetModalPassive w = wrapMRPassive (windowGetModal w)


-- @G: windowGetOpacity					 | ["window","Get","Opacity"]
windowGetOpacityPassive :: WindowClass self => (self) -> ReactiveFieldRead IO (Double)
windowGetOpacityPassive w = wrapMRPassive (windowGetOpacity w)


-- @G: windowGetPosition					 | ["window","Get","Position"]
windowGetPositionPassive :: WindowClass self => (self) -> ReactiveFieldRead IO ((Int, Int))
windowGetPositionPassive w = wrapMRPassive (windowGetPosition w)


-- @G: windowGetResizable					 | ["window","Get","Resizable"]
windowGetResizablePassive :: WindowClass self => (self) -> ReactiveFieldRead IO (Bool)
windowGetResizablePassive w = wrapMRPassive (windowGetResizable w)


-- @G: windowGetRole					 | ["window","Get","Role"]
windowGetRolePassive :: (WindowClass self, GlibString string) => (self) -> ReactiveFieldRead IO ((Maybe string))
windowGetRolePassive w = wrapMRPassive (windowGetRole w)


-- @G: windowGetScreen					 | ["window","Get","Screen"]
windowGetScreenPassive :: WindowClass self => (self) -> ReactiveFieldRead IO (Screen)
windowGetScreenPassive w = wrapMRPassive (windowGetScreen w)


-- @G: windowGetSize					 | ["window","Get","Size"]
windowGetSizePassive :: WindowClass self => (self) -> ReactiveFieldRead IO ((Int, Int))
windowGetSizePassive w = wrapMRPassive (windowGetSize w)


-- @G: windowGetSkipPagerHint					 | ["window","Get","Skip","Pager","Hint"]
windowGetSkipPagerHintPassive :: WindowClass self => (self) -> ReactiveFieldRead IO (Bool)
windowGetSkipPagerHintPassive w = wrapMRPassive (windowGetSkipPagerHint w)


-- @G: windowGetSkipTaskbarHint					 | ["window","Get","Skip","Taskbar","Hint"]
windowGetSkipTaskbarHintPassive :: WindowClass self => (self) -> ReactiveFieldRead IO (Bool)
windowGetSkipTaskbarHintPassive w = wrapMRPassive (windowGetSkipTaskbarHint w)


-- @G: windowGetTitle					 | ["window","Get","Title"]
windowGetTitlePassive :: (WindowClass self, GlibString string) => (self) -> ReactiveFieldRead IO (string)
windowGetTitlePassive w = wrapMRPassive (windowGetTitle w)


-- @G: windowGetTransientFor					 | ["window","Get","Transient","For"]
windowGetTransientForPassive :: WindowClass self => (self) -> ReactiveFieldRead IO ((Maybe Window))
windowGetTransientForPassive w = wrapMRPassive (windowGetTransientFor w)


-- @G: windowGetTypeHint					 | ["window","Get","Type","Hint"]
windowGetTypeHintPassive :: WindowClass self => (self) -> ReactiveFieldRead IO (WindowTypeHint)
windowGetTypeHintPassive w = wrapMRPassive (windowGetTypeHint w)


-- @G: windowGetUrgencyHint					 | ["window","Get","Urgency","Hint"]
windowGetUrgencyHintPassive :: WindowClass self => (self) -> ReactiveFieldRead IO (Bool)
windowGetUrgencyHintPassive w = wrapMRPassive (windowGetUrgencyHint w)


-- @G: windowGetWindowType					 | ["window","Get","Window","Type"]
windowGetWindowTypePassive :: WindowClass self => (self) -> ReactiveFieldRead IO (WindowType)
windowGetWindowTypePassive w = wrapMRPassive (windowGetWindowType w)


-- @A: windowGravity
windowGravityPassive :: WindowClass self => (self) -> ReactiveFieldReadWrite IO (Gravity)
windowGravityPassive w = passivePropertyNE w windowGravity


-- @A: windowHasFrame
windowHasFramePassive :: WindowClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
windowHasFramePassive w = passivePropertyNE w windowHasFrame


-- @A: windowIcon
windowIconPassive :: WindowClass self => (self) -> ReactiveFieldReadWrite IO ((Maybe Pixbuf))
windowIconPassive w = passivePropertyNE w windowIcon


-- @A: windowIconList
windowIconListPassive :: WindowClass self => (self) -> ReactiveFieldReadWrite IO ([Pixbuf])
windowIconListPassive w = passivePropertyNE w windowIconList


-- @A: windowIconName
windowIconNamePassive :: (WindowClass self, GlibString string) => (self) -> ReactiveFieldReadWrite IO (string)
windowIconNamePassive w = passivePropertyNE w windowIconName


-- @A: windowMnemonicModifier
windowMnemonicModifierPassive :: WindowClass self => (self) -> ReactiveFieldReadWrite IO ([Modifier])
windowMnemonicModifierPassive w = passivePropertyNE w windowMnemonicModifier


-- @A: windowMnemonicVisible
windowMnemonicVisiblePassive :: WindowClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
windowMnemonicVisiblePassive w = passivePropertyNE w windowMnemonicVisible


-- @A: windowModal
windowModalPassive :: WindowClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
windowModalPassive w = passivePropertyNE w windowModal


-- @A: windowOpacity
windowOpacityPassive :: WindowClass self => (self) -> ReactiveFieldReadWrite IO (Double)
windowOpacityPassive w = passivePropertyNE w windowOpacity


-- @A: windowResizable
windowResizablePassive :: WindowClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
windowResizablePassive w = passivePropertyNE w windowResizable


-- @A: windowRole
windowRolePassive :: (WindowClass self, GlibString string) => (self) -> ReactiveFieldReadWrite IO (string)
windowRolePassive w = passivePropertyNE w windowRole


-- @A: windowScreen
windowScreenPassive :: WindowClass self => (self) -> ReactiveFieldReadWrite IO (Screen)
windowScreenPassive w = passivePropertyNE w windowScreen


-- @T: windowSetAcceptFocus					 | ["window","Set","Accept","Focus"]
windowSetAcceptFocusReactive :: WindowClass self => (self) -> ReactiveFieldWrite IO (Bool)
windowSetAcceptFocusReactive w = wrapMW (windowSetAcceptFocus w)


-- @T: windowSetAutoStartupNotification					 | ["window","Set","Auto","Startup","Notification"]
-- TODO
-- @T: windowSetDecorated					 | ["window","Set","Decorated"]
windowSetDecoratedReactive :: WindowClass self => (self) -> ReactiveFieldWrite IO (Bool)
windowSetDecoratedReactive w = wrapMW (windowSetDecorated w)


-- @T: windowSetDefault					 | ["window","Set","Default"]
windowSetDefaultReactive :: (WindowClass self, WidgetClass widget) => (self) -> ReactiveFieldWrite IO (Maybe widget)
windowSetDefaultReactive w = wrapMW (windowSetDefault w)


-- @T: windowSetDefaultIconFromFile					 | ["window","Set","Default","Icon","From","File"]
-- TODO
-- @T: windowSetDefaultIconList					 | ["window","Set","Default","Icon","List"]
-- TODO
-- @T: windowSetDefaultIcon					 | ["window","Set","Default","Icon"]
-- TODO
-- @T: windowSetDefaultIconName					 | ["window","Set","Default","Icon","Name"]
-- TODO
-- @T: windowSetDefaultSize					 | ["window","Set","Default","Size"]
-- TODO
-- @T: windowSetDeletable					 | ["window","Set","Deletable"]
windowSetDeletableReactive :: WindowClass self => (self) -> ReactiveFieldWrite IO (Bool)
windowSetDeletableReactive w = wrapMW (windowSetDeletable w)


-- @T: windowSetDestroyWithParent					 | ["window","Set","Destroy","With","Parent"]
windowSetDestroyWithParentReactive :: WindowClass self => (self) -> ReactiveFieldWrite IO (Bool)
windowSetDestroyWithParentReactive w = wrapMW (windowSetDestroyWithParent w)


-- @T: windowSetFocus					 | ["window","Set","Focus"]
windowSetFocusReactive :: (WindowClass self, WidgetClass widget) => (self) -> ReactiveFieldWrite IO (Maybe widget)
windowSetFocusReactive w = wrapMW (windowSetFocus w)


-- @T: windowSetFocusOnMap					 | ["window","Set","Focus","On","Map"]
windowSetFocusOnMapReactive :: WindowClass self => (self) -> ReactiveFieldWrite IO (Bool)
windowSetFocusOnMapReactive w = wrapMW (windowSetFocusOnMap w)


-- @T: windowSetFrameDimensions					 | ["window","Set","Frame","Dimensions"]
-- TODO
-- @T: windowSetGeometryHints					 | ["window","Set","Geometry","Hints"]
-- TODO
-- @T: windowSetGravity					 | ["window","Set","Gravity"]
windowSetGravityReactive :: WindowClass self => (self) -> ReactiveFieldWrite IO (Gravity)
windowSetGravityReactive w = wrapMW (windowSetGravity w)


-- @T: windowSetHasFrame					 | ["window","Set","Has","Frame"]
windowSetHasFrameReactive :: WindowClass self => (self) -> ReactiveFieldWrite IO (Bool)
windowSetHasFrameReactive w = wrapMW (windowSetHasFrame w)


-- @T: windowSetIconFromFile					 | ["window","Set","Icon","From","File"]
windowSetIconFromFileReactive :: (WindowClass self, GlibFilePath fp) => (self) -> ReactiveFieldWrite IO (fp)
windowSetIconFromFileReactive w = wrapMW (windowSetIconFromFile w)


-- @T: windowSetIcon					 | ["window","Set","Icon"]
windowSetIconReactive :: WindowClass self => (self) -> ReactiveFieldWrite IO (Maybe Pixbuf)
windowSetIconReactive w = wrapMW (windowSetIcon w)


-- @T: windowSetIconList					 | ["window","Set","Icon","List"]
windowSetIconListReactive :: WindowClass self => (self) -> ReactiveFieldWrite IO ([Pixbuf])
windowSetIconListReactive w = wrapMW (windowSetIconList w)


-- @T: windowSetIconName					 | ["window","Set","Icon","Name"]
windowSetIconNameReactive :: (WindowClass self, GlibString string) => (self) -> ReactiveFieldWrite IO (string)
windowSetIconNameReactive w = wrapMW (windowSetIconName w)


-- @T: windowSetKeepAbove					 | ["window","Set","Keep","Above"]
windowSetKeepAboveReactive :: WindowClass self => (self) -> ReactiveFieldWrite IO (Bool)
windowSetKeepAboveReactive w = wrapMW (windowSetKeepAbove w)


-- @T: windowSetKeepBelow					 | ["window","Set","Keep","Below"]
windowSetKeepBelowReactive :: WindowClass self => (self) -> ReactiveFieldWrite IO (Bool)
windowSetKeepBelowReactive w = wrapMW (windowSetKeepBelow w)


-- @T: windowSetMnemonicModifier					 | ["window","Set","Mnemonic","Modifier"]
windowSetMnemonicModifierReactive :: WindowClass self => (self) -> ReactiveFieldWrite IO ([Modifier])
windowSetMnemonicModifierReactive w = wrapMW (windowSetMnemonicModifier w)


-- @T: windowSetModal					 | ["window","Set","Modal"]
windowSetModalReactive :: WindowClass self => (self) -> ReactiveFieldWrite IO (Bool)
windowSetModalReactive w = wrapMW (windowSetModal w)


-- @T: windowSetOpacity					 | ["window","Set","Opacity"]
windowSetOpacityReactive :: WindowClass self => (self) -> ReactiveFieldWrite IO (Double)
windowSetOpacityReactive w = wrapMW (windowSetOpacity w)


-- @T: windowSetPolicy					 | ["window","Set","Policy"]
-- TODO
-- @T: windowSetPosition					 | ["window","Set","Position"]
windowSetPositionReactive :: WindowClass self => (self) -> ReactiveFieldWrite IO (WindowPosition)
windowSetPositionReactive w = wrapMW (windowSetPosition w)


-- @T: windowSetResizable					 | ["window","Set","Resizable"]
windowSetResizableReactive :: WindowClass self => (self) -> ReactiveFieldWrite IO (Bool)
windowSetResizableReactive w = wrapMW (windowSetResizable w)


-- @T: windowSetRole					 | ["window","Set","Role"]
windowSetRoleReactive :: (WindowClass self, GlibString string) => (self) -> ReactiveFieldWrite IO (string)
windowSetRoleReactive w = wrapMW (windowSetRole w)


-- @T: windowSetScreen					 | ["window","Set","Screen"]
windowSetScreenReactive :: WindowClass self => (self) -> ReactiveFieldWrite IO (Screen)
windowSetScreenReactive w = wrapMW (windowSetScreen w)


-- @T: windowSetSkipPagerHint					 | ["window","Set","Skip","Pager","Hint"]
windowSetSkipPagerHintReactive :: WindowClass self => (self) -> ReactiveFieldWrite IO (Bool)
windowSetSkipPagerHintReactive w = wrapMW (windowSetSkipPagerHint w)


-- @T: windowSetSkipTaskbarHint					 | ["window","Set","Skip","Taskbar","Hint"]
windowSetSkipTaskbarHintReactive :: WindowClass self => (self) -> ReactiveFieldWrite IO (Bool)
windowSetSkipTaskbarHintReactive w = wrapMW (windowSetSkipTaskbarHint w)


-- @T: windowSetStartupId					 | ["window","Set","Startup","Id"]
windowSetStartupIdReactive :: (WindowClass self, GlibString string) => (self) -> ReactiveFieldWrite IO (string)
windowSetStartupIdReactive w = wrapMW (windowSetStartupId w)


-- @T: windowSetTitle					 | ["window","Set","Title"]
windowSetTitleReactive :: (WindowClass self, GlibString string) => (self) -> ReactiveFieldWrite IO (string)
windowSetTitleReactive w = wrapMW (windowSetTitle w)


-- @T: windowSetTransientFor					 | ["window","Set","Transient","For"]
windowSetTransientForReactive :: (WindowClass self, WindowClass parent) => (self) -> ReactiveFieldWrite IO (parent)
windowSetTransientForReactive w = wrapMW (windowSetTransientFor w)


-- @T: windowSetTypeHint					 | ["window","Set","Type","Hint"]
windowSetTypeHintReactive :: WindowClass self => (self) -> ReactiveFieldWrite IO (WindowTypeHint)
windowSetTypeHintReactive w = wrapMW (windowSetTypeHint w)


-- @T: windowSetUrgencyHint					 | ["window","Set","Urgency","Hint"]
windowSetUrgencyHintReactive :: WindowClass self => (self) -> ReactiveFieldWrite IO (Bool)
windowSetUrgencyHintReactive w = wrapMW (windowSetUrgencyHint w)


-- @A: windowSkipPagerHint
windowSkipPagerHintPassive :: WindowClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
windowSkipPagerHintPassive w = passivePropertyNE w windowSkipPagerHint


-- @A: windowSkipTaskbarHint
windowSkipTaskbarHintPassive :: WindowClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
windowSkipTaskbarHintPassive w = passivePropertyNE w windowSkipTaskbarHint


-- @A: windowStartupId
windowStartupIdPassive :: (WindowClass self, GlibString string) => (self) -> ReactiveFieldReadWrite IO (string)
windowStartupIdPassive w = passivePropertyNE w windowStartupId


-- @A: windowTitle
windowTitlePassive :: (WindowClass self, GlibString string) => (self) -> ReactiveFieldReadWrite IO (string)
windowTitlePassive w = passivePropertyNE w windowTitle


-- @A: windowToplevelFocus
windowToplevelFocusPassive :: WindowClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
windowToplevelFocusPassive w = passivePropertyNE w windowToplevelFocus


-- @A: windowTypeHint
windowTypeHintPassive :: WindowClass self => (self) -> ReactiveFieldReadWrite IO (WindowTypeHint)
windowTypeHintPassive w = passivePropertyNE w windowTypeHint


-- @A: windowUrgencyHint
windowUrgencyHintPassive :: WindowClass self => (self) -> ReactiveFieldReadWrite IO (Bool)
windowUrgencyHintPassive w = passivePropertyNE w windowUrgencyHint


-- @A: windowWindowPosition
windowWindowPositionPassive :: WindowClass self => (self) -> ReactiveFieldReadWrite IO (WindowPosition)
windowWindowPositionPassive w = passivePropertyNE w windowWindowPosition


-- @G: fontDescriptionGetFamily					 | ["font","Description","Get","Family"]
fontDescriptionGetFamilyPassive :: GlibString string => (FontDescription) -> ReactiveFieldRead IO ((Maybe string))
fontDescriptionGetFamilyPassive w = wrapMRPassive (fontDescriptionGetFamily w)


-- @G: fontDescriptionGetSize					 | ["font","Description","Get","Size"]
fontDescriptionGetSizePassive :: (FontDescription) -> ReactiveFieldRead IO ((Maybe Double))
fontDescriptionGetSizePassive w = wrapMRPassive (fontDescriptionGetSize w)


-- @G: fontDescriptionGetStretch					 | ["font","Description","Get","Stretch"]
fontDescriptionGetStretchPassive :: (FontDescription) -> ReactiveFieldRead IO ((Maybe Stretch))
fontDescriptionGetStretchPassive w = wrapMRPassive (fontDescriptionGetStretch w)


-- @G: fontDescriptionGetStyle					 | ["font","Description","Get","Style"]
fontDescriptionGetStylePassive :: (FontDescription) -> ReactiveFieldRead IO ((Maybe FontStyle))
fontDescriptionGetStylePassive w = wrapMRPassive (fontDescriptionGetStyle w)


-- @G: fontDescriptionGetVariant					 | ["font","Description","Get","Variant"]
fontDescriptionGetVariantPassive :: (FontDescription) -> ReactiveFieldRead IO ((Maybe Variant))
fontDescriptionGetVariantPassive w = wrapMRPassive (fontDescriptionGetVariant w)


-- @G: fontDescriptionGetWeight					 | ["font","Description","Get","Weight"]
fontDescriptionGetWeightPassive :: (FontDescription) -> ReactiveFieldRead IO ((Maybe Weight))
fontDescriptionGetWeightPassive w = wrapMRPassive (fontDescriptionGetWeight w)


-- @T: fontDescriptionSetFamily					 | ["font","Description","Set","Family"]
fontDescriptionSetFamilyReactive :: GlibString string => (FontDescription) -> ReactiveFieldWrite IO (string)
fontDescriptionSetFamilyReactive w = wrapMW (fontDescriptionSetFamily w)


-- @T: fontDescriptionSetSize					 | ["font","Description","Set","Size"]
fontDescriptionSetSizePassive :: (FontDescription) -> ReactiveFieldWrite IO (Double)
fontDescriptionSetSizePassive w = wrapMW (fontDescriptionSetSize w)

-- @T: fontDescriptionSetStretch					 | ["font","Description","Set","Stretch"]
fontDescriptionSetStretchPassive :: (FontDescription) -> ReactiveFieldWrite IO (Stretch)
fontDescriptionSetStretchPassive w = wrapMW (fontDescriptionSetStretch w)

-- @T: fontDescriptionSetStyle					 | ["font","Description","Set","Style"]
fontDescriptionSetStylePassive :: (FontDescription) -> ReactiveFieldWrite IO (FontStyle)
fontDescriptionSetStylePassive w = wrapMW (fontDescriptionSetStyle w)

-- @T: fontDescriptionSetVariant					 | ["font","Description","Set","Variant"]
fontDescriptionSetVariantPassive :: (FontDescription) -> ReactiveFieldWrite IO (Variant)
fontDescriptionSetVariantPassive w = wrapMW (fontDescriptionSetVariant w)

-- @T: fontDescriptionSetWeight					 | ["font","Description","Set","Weight"]
fontDescriptionSetWeightPassive :: (FontDescription) -> ReactiveFieldWrite IO (Weight)
fontDescriptionSetWeightPassive w = wrapMW (fontDescriptionSetWeight w)

-- @G: glyphItemGetLogicalWidths					 | ["glyph","Item","Get","Logical","Widths"]
-- TODO
