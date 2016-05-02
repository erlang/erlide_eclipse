package org.erlide.engine.model.news;

import org.eclipse.jdt.annotation.NonNull;
import org.erlide.engine.model.news.IErlangForm;

@SuppressWarnings("all")
public interface IErlangComment extends IErlangForm {
  /**
   * Comments on the same level on succesive lines are merged together.
   * %-signs are not included in the text.
   */
  @NonNull
  public abstract Iterable<String> getText();
  
  /**
   * The number of % signs in front of the comment
   */
  public abstract int getLevel();
}
