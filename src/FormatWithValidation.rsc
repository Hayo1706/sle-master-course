module FormatWithValidation

extend Format;
import util::SimpleBox;
import Syntax;

Box form2box((Question)`<Str question> <Id name> : <Type anstype> <ValidationRule val>`) = V(question, I(H(name, ":", anstype, val)));
