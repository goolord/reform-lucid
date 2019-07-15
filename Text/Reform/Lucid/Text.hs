{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, TypeFamilies, UndecidableInstances, ViewPatterns #-}
{- |
This module provides functions creating Reform using blaze-html markup.

This module assumes that you wish for text based controls such as 'inputText' and 'textarea' to using 'Text' values. If you prefer 'String' see "Text.Reform.Blaze.String".

-}
module Text.Reform.Lucid.Text
    ( -- * \<input\> element
      inputText
    , inputPassword
    , inputSubmit
    , inputReset
    , inputHidden
    , inputButton
    , C.inputCheckbox
    , C.inputCheckboxes
    , C.inputRadio
    , C.inputFile
      -- * \<textarea\> element
    , textarea
      -- * \<button\> element
    , buttonSubmit
    , C.buttonReset
    , C.button
      -- * \<select\> element
    , C.select
    , C.selectMultiple
      -- * \<label\> element
    , C.label
      -- * errors
    , C.errorList
    , C.childErrorList
      -- * layout functions
    , C.br
    , C.fieldset
    , C.ol
    , C.ul
    , C.li
    , C.formGenGET
    , C.formGenPOST
    ) where

import Data.Text (Text, empty)
import Lucid (ToHtml, HtmlT)
import Text.Reform
import qualified Text.Reform.Lucid.Common as C

-- | Create an @\<input type=\"text\"\>@ element
inputText :: (Monad m, FormInput input, FormError error, ErrorInputType error ~ input, Applicative f) =>
               Text -- ^ initial value
            -> Form m input error (HtmlT f ()) () Text
inputText initialValue = C.inputText getInputText initialValue

-- | Create an @\<input type=\"password\"\>@ element
inputPassword :: (Monad m, FormInput input, FormError error, ErrorInputType error ~ input, Applicative f) =>
                 Form m input error (HtmlT f ()) () Text
inputPassword = C.inputPassword getInputText empty

-- | Create an @\<input type=\"submit\"\>@ element
--
-- returns:
--
--   [@Just@ /value/] if this button was used to submit the form.
--
--   [@Nothing@]    if this button was not used to submit the form.
inputSubmit :: (Monad m, FormInput input, FormError error, ErrorInputType error ~ input, Applicative f) =>
               Text -- ^ @value@ attribute. Used for button label, and value if button is submitted.
            -> Form m input error (HtmlT f ()) () (Maybe Text)
inputSubmit initialValue = C.inputSubmit getInputText initialValue

-- | Create an @\<input type=\"reset\"\>@ element
--
-- This element does not add any data to the form data set.
inputReset :: (Monad m, FormInput input, FormError error, ErrorInputType error ~ input, Applicative f) =>
              Text -- ^ value attribute. Used only to label the button.
           -> Form m input error (HtmlT f ()) () ()
inputReset = C.inputReset

-- | Create an @\<input type=\"hidden\"\>@ element
inputHidden :: (Monad m, FormInput input, FormError error, ErrorInputType error ~ input, Applicative f) =>
               Text -- ^ value to store in the hidden element
            -> Form m input error (HtmlT f ()) () Text
inputHidden initialValue = C.inputHidden getInputText initialValue

-- | Create an @\<input type=\"button\"\>@ element
--
-- The element is a push button with a text label. The button does nothing by default, but actions can be added using javascript. This element does not add any data to the form data set.
--
-- see also: 'C.button'
inputButton :: (Monad m, FormInput input, FormError error, ErrorInputType error ~ input, Applicative f) =>
               Text -- ^ value attribute. Used to label the button.
            -> Form m input error (HtmlT f ()) () ()
inputButton label = C.inputButton label

-- | Create a \<textarea\>\<\/textarea\> element
textarea :: (Monad m, FormInput input, FormError error, ErrorInputType error ~ input, Monad f) =>
            Int    -- ^ cols
         -> Int    -- ^ rows
         -> Text -- ^ initial contents
         -> Form m input error (HtmlT f ()) () Text
textarea rows cols initialValue = C.textarea getInputText rows cols initialValue

-- | create a  @\<button type=\"submit\"\>\<\/button\>@ element
buttonSubmit :: ( Monad m, FormError error, FormInput input, ErrorInputType error ~ input, ToHtml children, Monad f) =>
                Text -- ^ value attribute. Returned if this button submits the form.
             -> children -- ^ children to embed in the \<button\>
             -> Form m input error (HtmlT f ()) () (Maybe Text)
buttonSubmit = C.buttonSubmit getInputText
