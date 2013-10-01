package org.erlide.core.internal.builder;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;

@SuppressWarnings("all")
public class ErlangToolExtensions {
  public static boolean hasRebarConfig(final IProject project) {
    return false;
  }
  
  public static boolean hasMakefile(final IProject project) {
    return false;
  }
  
  public static boolean hasConcreteMk(final IProject project) {
    return false;
  }
  
  public static boolean hasErlangMk(final IProject project) {
    return false;
  }
  
  public static boolean isUniversalMake(final IFile makefile) {
    return false;
  }
}
