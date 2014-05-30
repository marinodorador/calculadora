package lexic

abstract class LexicCategorie {
  var lexeme: String = null;
}

class Final extends LexicCategorie {
  def this(lexeme: String) {
    this()
    this.lexeme = lexeme;
  }
}

class Number extends LexicCategorie {
  var value: Double = _
  def this(lexeme: String, value: Double) {
    this();
    this.lexeme = lexeme;
    this.value = value;
  }
  override def toString(): String = {
    return lexeme + "(" + value + ")"
  }
}

class Group extends LexicCategorie {
  def this(lexeme: String) {
    this()
    this.lexeme = lexeme;
  }
  override def toString(): String = {
    return lexeme
  }
}

class Identifier extends LexicCategorie {
  def this(lexeme: String) {
    this()
    this.lexeme = lexeme;
  }
  override def toString(): String = {
    lexeme
  }
}

class Opad extends LexicCategorie {
  def this(lexeme: String) {
    this()
    this.lexeme = lexeme;
  }
  override def toString(): String = {
    lexeme
  }
}

class Opml extends LexicCategorie {
  def this(lexeme: String) {
    this()
    this.lexeme = lexeme;
  }
  override def toString(): String = {
    lexeme
  }
}

class Pow extends LexicCategorie {
  def this(lexeme: String) {
    this()
    this.lexeme = lexeme;
  }
  override def toString(): String = {
    lexeme
  }
}

class Separator extends LexicCategorie {
  def this(lexeme: String) {
    this()
    this.lexeme = lexeme;
  }
  override def toString(): String = {
    lexeme
  }
}

