module Action where
import Model

type Action = Tick Float | Input String | Reset | ChangeLanguage Model.Language
