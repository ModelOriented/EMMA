# -WUM-Teksty    
Zbiór ma następującą strukturę: Księga == Wiersz, Słowo == Kolumna

Problem z tym zbiorem polega na dysproporcji pomiędzy ilością kolumn i wierszy - kolumn jest więcej. Z powodu zbyt małej liczby obserwacji względem ilości zmiennych objaśniających żaden model nie zaakceptuje takich danych w procesie uczenia.  
Wyjściem z tej sytuacji jest albo powyrzucanie słów, które w ogólności pojawiają się w małej ilości ksiąg, ale jest ono odradzane przez prowadzącego.  
Wg Michała lepiej zastosować embeding (zanurzenie).   
Metodologia: Słowa (kolumny) -> wektory; księga - liczność słowa - średni wektor.  

Narzędzia: biblioteka GloVe, paczka flair.  
Warto zastosować spłaszczenie - logarytmiczne, pierwiastkowe.  
Nie wyrzucać żadkich słów, one mogą być najbardziej informatywne, warto pozbyć się słów najczęstszych typu 'jest'.  
# Terminy:  
5.05 - I kamień milowy -> feature engineering, skalowanie!, kodowanie, etc.  
26.05 - II kamień milowy -> ?  
9.06 - III kamień milowy -> ?
