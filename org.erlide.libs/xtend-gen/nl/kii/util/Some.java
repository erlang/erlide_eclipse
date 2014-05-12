package nl.kii.util;

import com.google.common.base.Objects;
import java.util.Iterator;
import java.util.LinkedList;
import nl.kii.util.Opt;
import org.eclipse.xtend2.lib.StringConcatenation;
import org.eclipse.xtext.xbase.lib.CollectionLiterals;

@SuppressWarnings("all")
public class Some<T extends Object> extends Opt<T> {
  private T value;
  
  public Some(final T value) {
    boolean _tripleEquals = (value == null);
    if (_tripleEquals) {
      throw new NullPointerException("cannot create new Some(null)");
    }
    this.value = value;
  }
  
  public T value() {
    return this.value;
  }
  
  public boolean hasSome() {
    return true;
  }
  
  public boolean hasNone() {
    return false;
  }
  
  public boolean hasError() {
    return false;
  }
  
  public Iterator<T> iterator() {
    LinkedList<T> _newLinkedList = CollectionLiterals.<T>newLinkedList(this.value);
    return _newLinkedList.iterator();
  }
  
  public int hashCode() {
    return this.value.hashCode();
  }
  
  public boolean equals(final Object obj) {
    boolean _or = false;
    boolean _equals = Objects.equal(obj, this.value);
    if (_equals) {
      _or = true;
    } else {
      boolean _and = false;
      if (!(obj instanceof Some<?>)) {
        _and = false;
      } else {
        boolean _equals_1 = Objects.equal(((Some<?>) obj).value, this.value);
        _and = _equals_1;
      }
      _or = _and;
    }
    return _or;
  }
  
  public String toString() {
    StringConcatenation _builder = new StringConcatenation();
    _builder.append("Some(");
    _builder.append(this.value, "");
    _builder.append(")");
    return _builder.toString();
  }
}
