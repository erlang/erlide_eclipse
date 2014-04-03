/**
 * Copyright (c) 2014 Joerg Reichert
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     Joerg Reichert
 */
package org.erlide.annotations;

import java.io.IOException;
import java.io.InputStream;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.MissingResourceException;
import java.util.Properties;
import java.util.ResourceBundle;
import java.util.Set;
import java.util.regex.MatchResult;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.eclipse.xtend.lib.macro.AbstractClassProcessor;
import org.eclipse.xtend.lib.macro.TransformationContext;
import org.eclipse.xtend.lib.macro.declaration.CompilationStrategy;
import org.eclipse.xtend.lib.macro.declaration.CompilationUnit;
import org.eclipse.xtend.lib.macro.declaration.MutableAnnotationReference;
import org.eclipse.xtend.lib.macro.declaration.MutableClassDeclaration;
import org.eclipse.xtend.lib.macro.declaration.MutableFieldDeclaration;
import org.eclipse.xtend.lib.macro.declaration.MutableMethodDeclaration;
import org.eclipse.xtend.lib.macro.declaration.MutableParameterDeclaration;
import org.eclipse.xtend.lib.macro.declaration.Type;
import org.eclipse.xtend.lib.macro.declaration.TypeReference;
import org.eclipse.xtend.lib.macro.declaration.Visibility;
import org.eclipse.xtend.lib.macro.file.Path;
import org.eclipse.xtend2.lib.StringConcatenation;
import org.eclipse.xtext.xbase.lib.CollectionLiterals;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.eclipse.xtext.xbase.lib.Extension;
import org.eclipse.xtext.xbase.lib.Functions.Function0;
import org.eclipse.xtext.xbase.lib.Functions.Function1;
import org.eclipse.xtext.xbase.lib.IntegerRange;
import org.eclipse.xtext.xbase.lib.IterableExtensions;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure1;
import org.eclipse.xtext.xbase.lib.StringExtensions;
import org.erlide.annotations.NLS;

@SuppressWarnings("all")
public class NLSProcessor extends AbstractClassProcessor {
  private final static String BUNDLE_NAME_FIELD = "BUNDLE_NAME";
  
  private final static String RESOURCE_BUNDLE_FIELD = "RESOURCE_BUNDLE";
  
  private final static String nlsClass = "org.eclipse.osgi.util.NLS";
  
  public void doTransform(final MutableClassDeclaration annotatedClass, @Extension final TransformationContext context) {
    final MutableAnnotationReference nlsAnnotation = this.getNLSAnnotation(annotatedClass, context);
    Type _findTypeGlobally = context.findTypeGlobally(NLSProcessor.nlsClass);
    boolean _tripleEquals = (_findTypeGlobally == null);
    if (_tripleEquals) {
      context.addError(nlsAnnotation, (NLSProcessor.nlsClass + " isn\'t on the classpath."));
    }
    final String propertyFileNameValue = this.getNLSAnnotationPropertyValue(nlsAnnotation, context);
    final Path propertiesFile = this.getPropertiesFile(annotatedClass, context, propertyFileNameValue, nlsAnnotation);
    boolean _tripleNotEquals = (propertiesFile != null);
    if (_tripleNotEquals) {
      final InputStream propertiesFileInputStream = context.getContentsAsStream(propertiesFile);
      final Properties properties = this.loadPropertiesFile(propertiesFileInputStream, context, nlsAnnotation);
      this.addBundleNameField(annotatedClass, nlsAnnotation, context, propertyFileNameValue);
      this.addResourceBundleField(annotatedClass, nlsAnnotation, context);
      this.addStaticBlock(annotatedClass, nlsAnnotation, context);
      this.addGetStringMethod(annotatedClass, nlsAnnotation, context);
      Set<Map.Entry<Object,Object>> _entrySet = properties.entrySet();
      final Function1<Map.Entry<Object,Object>,String> _function = new Function1<Map.Entry<Object,Object>,String>() {
        public String apply(final Map.Entry<Object,Object> it) {
          Object _key = it.getKey();
          return String.valueOf(_key);
        }
      };
      List<Map.Entry<Object,Object>> _sortBy = IterableExtensions.<Map.Entry<Object,Object>, String>sortBy(_entrySet, _function);
      final Procedure1<Map.Entry<Object,Object>> _function_1 = new Procedure1<Map.Entry<Object,Object>>() {
        public void apply(final Map.Entry<Object,Object> it) {
          NLSProcessor.this.addField(it, annotatedClass, nlsAnnotation, context);
          NLSProcessor.this.addMethod(it, annotatedClass, nlsAnnotation, context);
        }
      };
      IterableExtensions.<Map.Entry<Object,Object>>forEach(_sortBy, _function_1);
    }
  }
  
  private void addStaticBlock(final MutableClassDeclaration annotatedClass, final MutableAnnotationReference nlsAnnotation, @Extension final TransformationContext context) {
    final String fieldName = "INITIALIZER";
    this.checkForExistentField(annotatedClass, fieldName, context, nlsAnnotation);
    final Procedure1<MutableFieldDeclaration> _function = new Procedure1<MutableFieldDeclaration>() {
      public void apply(final MutableFieldDeclaration it) {
        it.setVisibility(Visibility.PRIVATE);
        it.setStatic(true);
        it.setFinal(true);
        TypeReference _string = context.getString();
        it.setType(_string);
        final CompilationStrategy _function = new CompilationStrategy() {
          public CharSequence compile(final CompilationStrategy.CompilationContext it) {
            StringConcatenation _builder = new StringConcatenation();
            _builder.append("new ");
            TypeReference _string = context.getString();
            TypeReference _newTypeReference = context.newTypeReference(Function0.class, _string);
            String _javaCode = it.toJavaCode(_newTypeReference);
            _builder.append(_javaCode, "");
            _builder.append("() {");
            _builder.newLineIfNotEmpty();
            _builder.append("    ");
            _builder.append("public ");
            TypeReference _string_1 = context.getString();
            _builder.append(_string_1, "    ");
            _builder.append(" apply() {");
            _builder.newLineIfNotEmpty();
            _builder.append("      ");
            TypeReference _newTypeReference_1 = context.newTypeReference(NLSProcessor.nlsClass);
            String _javaCode_1 = it.toJavaCode(_newTypeReference_1);
            _builder.append(_javaCode_1, "      ");
            _builder.append(".initializeMessages(");
            MutableFieldDeclaration _findDeclaredField = annotatedClass.findDeclaredField(
              NLSProcessor.BUNDLE_NAME_FIELD);
            String _simpleName = _findDeclaredField.getSimpleName();
            _builder.append(_simpleName, "      ");
            _builder.append(", ");
            String _qualifiedName = annotatedClass.getQualifiedName();
            TypeReference _newTypeReference_2 = context.newTypeReference(_qualifiedName);
            String _javaCode_2 = it.toJavaCode(_newTypeReference_2);
            _builder.append(_javaCode_2, "      ");
            _builder.append(".class);");
            _builder.newLineIfNotEmpty();
            _builder.append("      ");
            _builder.append("return \"\";");
            _builder.newLine();
            _builder.append("    ");
            _builder.append("}");
            _builder.newLine();
            _builder.append("  ");
            _builder.append("}.apply();");
            _builder.newLine();
            return _builder;
          }
        };
        it.setInitializer(_function);
      }
    };
    annotatedClass.addField(fieldName, _function);
  }
  
  private void addBundleNameField(final MutableClassDeclaration annotatedClass, final MutableAnnotationReference nlsAnnotation, @Extension final TransformationContext context, final String propertyFileName) {
    this.checkForExistentField(annotatedClass, NLSProcessor.BUNDLE_NAME_FIELD, context, nlsAnnotation);
    final Procedure1<MutableFieldDeclaration> _function = new Procedure1<MutableFieldDeclaration>() {
      public void apply(final MutableFieldDeclaration it) {
        it.setVisibility(Visibility.PRIVATE);
        it.setStatic(true);
        it.setFinal(true);
        TypeReference _string = context.getString();
        it.setType(_string);
        final CompilationStrategy _function = new CompilationStrategy() {
          public CharSequence compile(final CompilationStrategy.CompilationContext it) {
            StringConcatenation _builder = new StringConcatenation();
            String _qualifiedName = annotatedClass.getQualifiedName();
            TypeReference _newTypeReference = context.newTypeReference(_qualifiedName);
            String _javaCode = it.toJavaCode(_newTypeReference);
            _builder.append(_javaCode, "");
            _builder.append(".class.getPackage().getName() + \".");
            String _replace = propertyFileName.replace(".properties", "");
            _builder.append(_replace, "");
            _builder.append("\"");
            return _builder;
          }
        };
        it.setInitializer(_function);
      }
    };
    annotatedClass.addField(NLSProcessor.BUNDLE_NAME_FIELD, _function);
  }
  
  private void addResourceBundleField(final MutableClassDeclaration annotatedClass, final MutableAnnotationReference nlsAnnotation, @Extension final TransformationContext context) {
    this.checkForExistentField(annotatedClass, NLSProcessor.RESOURCE_BUNDLE_FIELD, context, nlsAnnotation);
    final Procedure1<MutableFieldDeclaration> _function = new Procedure1<MutableFieldDeclaration>() {
      public void apply(final MutableFieldDeclaration it) {
        it.setVisibility(Visibility.PRIVATE);
        it.setStatic(true);
        it.setFinal(true);
        TypeReference _newTypeReference = context.newTypeReference(ResourceBundle.class);
        it.setType(_newTypeReference);
        final CompilationStrategy _function = new CompilationStrategy() {
          public CharSequence compile(final CompilationStrategy.CompilationContext it) {
            StringConcatenation _builder = new StringConcatenation();
            TypeReference _newTypeReference = context.newTypeReference(ResourceBundle.class);
            String _javaCode = it.toJavaCode(_newTypeReference);
            _builder.append(_javaCode, "");
            _builder.append(".getBundle(");
            MutableFieldDeclaration _findDeclaredField = annotatedClass.findDeclaredField(NLSProcessor.BUNDLE_NAME_FIELD);
            String _simpleName = _findDeclaredField.getSimpleName();
            _builder.append(_simpleName, "");
            _builder.append(")");
            return _builder;
          }
        };
        it.setInitializer(_function);
      }
    };
    annotatedClass.addField(NLSProcessor.RESOURCE_BUNDLE_FIELD, _function);
  }
  
  private void addGetStringMethod(final MutableClassDeclaration annotatedClass, final MutableAnnotationReference nlsAnnotation, @Extension final TransformationContext context) {
    final String methodName = "getString";
    this.checkForExistentMethod(annotatedClass, methodName, context, nlsAnnotation, 1);
    final Procedure1<MutableMethodDeclaration> _function = new Procedure1<MutableMethodDeclaration>() {
      public void apply(final MutableMethodDeclaration it) {
        it.setVisibility(Visibility.PRIVATE);
        it.setStatic(true);
        TypeReference _string = context.getString();
        it.setReturnType(_string);
        TypeReference _string_1 = context.getString();
        it.addParameter("key", _string_1);
        final CompilationStrategy _function = new CompilationStrategy() {
          public CharSequence compile(final CompilationStrategy.CompilationContext it) {
            StringConcatenation _builder = new StringConcatenation();
            _builder.append("try {");
            _builder.newLine();
            _builder.append("    ");
            _builder.append("return ");
            MutableFieldDeclaration _findDeclaredField = annotatedClass.findDeclaredField(NLSProcessor.RESOURCE_BUNDLE_FIELD);
            String _simpleName = _findDeclaredField.getSimpleName();
            _builder.append(_simpleName, "    ");
            _builder.append(".getString(key);");
            _builder.newLineIfNotEmpty();
            _builder.append("} catch (");
            TypeReference _newTypeReference = context.newTypeReference(MissingResourceException.class);
            String _javaCode = it.toJavaCode(_newTypeReference);
            _builder.append(_javaCode, "");
            _builder.append(" e) {");
            _builder.newLineIfNotEmpty();
            _builder.append("    ");
            _builder.append("return \'!\' + key + \'!\';");
            _builder.newLine();
            _builder.append("}");
            _builder.newLine();
            return _builder;
          }
        };
        it.setBody(_function);
      }
    };
    annotatedClass.addMethod(methodName, _function);
  }
  
  private void addField(final Map.Entry<Object,Object> entry, final MutableClassDeclaration annotatedClass, final MutableAnnotationReference nlsAnnotation, @Extension final TransformationContext context) {
    Object _key = entry.getKey();
    final String fieldName = ((String) _key);
    this.checkForExistentField(annotatedClass, fieldName, context, nlsAnnotation);
    final Procedure1<MutableFieldDeclaration> _function = new Procedure1<MutableFieldDeclaration>() {
      public void apply(final MutableFieldDeclaration it) {
        it.setVisibility(Visibility.PUBLIC);
        it.setStatic(true);
        it.setFinal(true);
        TypeReference _string = context.getString();
        it.setType(_string);
        final CompilationStrategy _function = new CompilationStrategy() {
          public CharSequence compile(final CompilationStrategy.CompilationContext it) {
            StringConcatenation _builder = new StringConcatenation();
            _builder.append("\"");
            Object _key = entry.getKey();
            _builder.append(_key, "");
            _builder.append("\"");
            return _builder;
          }
        };
        it.setInitializer(_function);
      }
    };
    annotatedClass.addField(fieldName, _function);
  }
  
  private void checkForExistentField(final MutableClassDeclaration annotatedClass, final String fieldName, @Extension final TransformationContext context, final MutableAnnotationReference nlsAnnotation) {
    MutableFieldDeclaration _findDeclaredField = annotatedClass.findDeclaredField(fieldName);
    boolean _tripleNotEquals = (_findDeclaredField != null);
    if (_tripleNotEquals) {
      context.addError(nlsAnnotation, (("Field " + fieldName) + " already present in class."));
    }
  }
  
  private void addMethod(final Map.Entry<Object,Object> entry, final MutableClassDeclaration annotatedClass, final MutableAnnotationReference nlsAnnotation, @Extension final TransformationContext context) {
    Object _value = entry.getValue();
    final String message = ((String) _value);
    final int wildcardCount = this.getWildcardCount(message);
    Iterable<String> _xifexpression = null;
    if ((wildcardCount > 0)) {
      IntegerRange _upTo = new IntegerRange(0, (wildcardCount - 1));
      final Function1<Integer,String> _function = new Function1<Integer,String>() {
        public String apply(final Integer it) {
          return ("param" + it);
        }
      };
      _xifexpression = IterableExtensions.<Integer, String>map(_upTo, _function);
    } else {
      _xifexpression = CollectionLiterals.<String>newArrayList();
    }
    final Iterable<String> params = _xifexpression;
    Object _key = entry.getKey();
    final String getMessageForMethodName = ("getMessageFor" + _key);
    int _size = IterableExtensions.size(params);
    this.checkForExistentMethod(annotatedClass, getMessageForMethodName, context, nlsAnnotation, _size);
    final Procedure1<MutableMethodDeclaration> _function_1 = new Procedure1<MutableMethodDeclaration>() {
      public void apply(final MutableMethodDeclaration it) {
        it.setVisibility(Visibility.PUBLIC);
        it.setStatic(true);
        TypeReference _string = context.getString();
        it.setReturnType(_string);
        final Procedure1<String> _function = new Procedure1<String>() {
          public void apply(final String param) {
            TypeReference _object = context.getObject();
            it.addParameter(param, _object);
          }
        };
        IterableExtensions.<String>forEach(params, _function);
        final CompilationStrategy _function_1 = new CompilationStrategy() {
          public CharSequence compile(final CompilationStrategy.CompilationContext it) {
            StringConcatenation _builder = new StringConcatenation();
            _builder.append("return ");
            TypeReference _newTypeReference = context.newTypeReference(NLSProcessor.nlsClass);
            String _javaCode = it.toJavaCode(_newTypeReference);
            _builder.append(_javaCode, "");
            _builder.append(".bind(getString(");
            Object _key = entry.getKey();
            _builder.append(_key, "");
            _builder.append("), new Object [] { ");
            String _join = IterableExtensions.join(params, ", ");
            _builder.append(_join, "");
            _builder.append(" });");
            return _builder;
          }
        };
        it.setBody(_function_1);
      }
    };
    annotatedClass.addMethod(getMessageForMethodName, _function_1);
  }
  
  private void checkForExistentMethod(final MutableClassDeclaration annotatedClass, final String methodName, @Extension final TransformationContext context, final MutableAnnotationReference nlsAnnotation, final int parameterListSize) {
    final MutableMethodDeclaration existentMethod = annotatedClass.findDeclaredMethod(methodName);
    boolean _tripleNotEquals = (existentMethod != null);
    if (_tripleNotEquals) {
      Iterable<? extends MutableParameterDeclaration> _parameters = existentMethod.getParameters();
      int _size = IterableExtensions.size(_parameters);
      boolean _equals = (_size == parameterListSize);
      if (_equals) {
        context.addError(nlsAnnotation, (((("Method " + methodName) + "/") + Integer.valueOf(parameterListSize)) + " already present in class."));
      }
    }
  }
  
  private int getWildcardCount(final String unboundMessage) {
    int _xblockexpression = (int) 0;
    {
      final Pattern pattern = Pattern.compile("\\{\\d*\\}");
      final Matcher matcher = pattern.matcher(unboundMessage);
      final HashSet<String> matches = CollectionLiterals.<String>newHashSet();
      boolean _find = matcher.find();
      boolean _while = _find;
      while (_while) {
        {
          final MatchResult matchResult = matcher.toMatchResult();
          String _group = matchResult.group();
          matches.add(_group);
        }
        boolean _find_1 = matcher.find();
        _while = _find_1;
      }
      _xblockexpression = matches.size();
    }
    return _xblockexpression;
  }
  
  private MutableAnnotationReference getNLSAnnotation(final MutableClassDeclaration annotatedClass, @Extension final TransformationContext context) {
    TypeReference _newTypeReference = context.newTypeReference(NLS.class);
    Type _type = _newTypeReference.getType();
    return annotatedClass.findAnnotation(_type);
  }
  
  private String getNLSAnnotationPropertyValue(final MutableAnnotationReference nlsAnnotation, @Extension final TransformationContext context) {
    String _xblockexpression = null;
    {
      Object _value = nlsAnnotation.getValue("propertyFileName");
      final String value = ((String) _value);
      boolean _isNullOrEmpty = StringExtensions.isNullOrEmpty(value);
      if (_isNullOrEmpty) {
        context.addError(nlsAnnotation, "@NLS requires non empty propertyFileName property value.");
      }
      _xblockexpression = value;
    }
    return _xblockexpression;
  }
  
  private Path getPropertiesFile(final MutableClassDeclaration annotatedClass, @Extension final TransformationContext context, final String propertyFileName, final MutableAnnotationReference nlsAnnotation) {
    Path _xblockexpression = null;
    {
      CompilationUnit _compilationUnit = annotatedClass.getCompilationUnit();
      Path _filePath = _compilationUnit.getFilePath();
      Path _parent = null;
      if (_filePath!=null) {
        _parent=_filePath.getParent();
      }
      final Path folder = _parent;
      boolean _or = false;
      boolean _tripleEquals = (folder == null);
      if (_tripleEquals) {
        _or = true;
      } else {
        boolean _exists = context.exists(folder);
        boolean _not = (!_exists);
        _or = _not;
      }
      if (_or) {
        String _qualifiedName = annotatedClass.getQualifiedName();
        String _plus = ("Cannot find folder for class " + _qualifiedName);
        String _plus_1 = (_plus + ": ");
        String _plus_2 = (_plus_1 + folder);
        context.addError(nlsAnnotation, _plus_2);
        String _qualifiedName_1 = annotatedClass.getQualifiedName();
        String _plus_3 = ("Cannot find folder for class " + _qualifiedName_1);
        String _plus_4 = (_plus_3 + ": ");
        String _plus_5 = (_plus_4 + folder);
        throw new IllegalArgumentException(_plus_5);
      }
      String _xifexpression = null;
      boolean _endsWith = propertyFileName.endsWith(".properties");
      boolean _not_1 = (!_endsWith);
      if (_not_1) {
        _xifexpression = ".properties";
      } else {
        _xifexpression = "";
      }
      String _plus_6 = (propertyFileName + _xifexpression);
      final Path propertiesFilePath = folder.append(_plus_6);
      Path _xifexpression_1 = null;
      boolean _exists_1 = context.exists(propertiesFilePath);
      boolean _not_2 = (!_exists_1);
      if (_not_2) {
        String _plus_7 = (propertiesFilePath + " doesn\'t exist.");
        context.addError(nlsAnnotation, _plus_7);
      } else {
        _xifexpression_1 = propertiesFilePath;
      }
      _xblockexpression = _xifexpression_1;
    }
    return _xblockexpression;
  }
  
  private Properties loadPropertiesFile(final InputStream propertiesFile, @Extension final TransformationContext transformationContext, final MutableAnnotationReference nlsAnnotation) {
    Properties _xblockexpression = null;
    {
      final Properties properties = new Properties();
      try {
        properties.load(propertiesFile);
      } catch (final Throwable _t) {
        if (_t instanceof IOException) {
          final IOException ioe = (IOException)_t;
          String _message = ioe.getMessage();
          String _plus = ("Cannot load properties file: " + _message);
          transformationContext.addError(nlsAnnotation, _plus);
        } else {
          throw Exceptions.sneakyThrow(_t);
        }
      }
      _xblockexpression = properties;
    }
    return _xblockexpression;
  }
}
