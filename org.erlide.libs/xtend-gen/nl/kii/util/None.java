package nl.kii.util;

import java.util.Iterator;
import java.util.LinkedList;
import nl.kii.util.NoneException;
import nl.kii.util.Opt;
import org.eclipse.xtend2.lib.StringConcatenation;
import org.eclipse.xtext.xbase.lib.CollectionLiterals;
import org.eclipse.xtext.xbase.lib.Exceptions;

@SuppressWarnings("all")
public class None<T extends Object> extends Opt<T> {
  public T value() {
    try {
      throw new NoneException();
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  public boolean hasSome() {
    return false;
  }
  
  public boolean hasNone() {
    return true;
  }
  
  public boolean hasError() {
    return false;
  }
  
  public Iterator<T> iterator() {
    LinkedList<T> _newLinkedList = CollectionLiterals.<T>newLinkedList();
    return _newLinkedList.iterator();
  }
  
  public boolean equals(final Object obj) {
    return (obj instanceof None<?>);
  }
  
  public int hashCode() {
    return 0;
  }
  
  public String toString() {
    StringConcatenation _builder = new StringConcatenation();
    _builder.append("None");
    return _builder.toString();
  }
}
