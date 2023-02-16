from pygments.styles.solarized import SolarizedLightStyle
from pygments.token import Keyword

class MyStyle(SolarizedLightStyle):
    styles = SolarizedLightStyle.styles
    styles[Keyword.Declaration] = styles[Token]
