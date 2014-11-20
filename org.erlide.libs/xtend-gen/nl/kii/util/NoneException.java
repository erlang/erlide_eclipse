package nl.kii.util;

@SuppressWarnings("all")
public class NoneException extends Exception {
  public NoneException() {
  }
  
  public NoneException(final String message) {
    super(message);
  }
}
