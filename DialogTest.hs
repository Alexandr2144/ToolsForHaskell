import Dialog.Class

import Dialog.Inline
import qualified Dialog.Edit as Edit
import qualified Dialog.List as List
import qualified Dialog.Prompt as Prompt


prompt_form_1 = Prompt.Form ["cmd"]
prompt_form_2 = Prompt.Form ["main","configuration"]
(Prompt.Popup prompt) = popup

edit_form_1 = Edit.Form Edit.Default "some value" ""
edit_form_2 = Edit.Form Edit.Secret "password" ""
(Edit.Popup edit) = popup

stdlist_form_1 = List.StdForm 1 "Select option" ["option A", "option B", "option C"]
(List.StdPopup list_popup) = popup

combolist_form_1 = List.ComboForm prompt_form_1
    "Select option" ["option A", "option B", "option C"]
(List.ComboPopup combo_popup) = popup
