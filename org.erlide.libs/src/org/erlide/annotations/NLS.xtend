/*******************************************************************************
 * Copyright (c) 2014 Joerg Reichert
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     Joerg Reichert
 *******************************************************************************/
package org.erlide.annotations

import java.io.IOException
import java.io.InputStream
import java.lang.annotation.ElementType
import java.lang.annotation.Target
import java.util.Map
import java.util.MissingResourceException
import java.util.Properties
import java.util.ResourceBundle
import java.util.regex.Pattern
import org.eclipse.xtend.lib.macro.AbstractClassProcessor
import org.eclipse.xtend.lib.macro.Active
import org.eclipse.xtend.lib.macro.TransformationContext
import org.eclipse.xtend.lib.macro.declaration.MutableAnnotationReference
import org.eclipse.xtend.lib.macro.declaration.MutableClassDeclaration
import org.eclipse.xtend.lib.macro.declaration.Visibility
import org.eclipse.xtext.xbase.lib.Functions.Function0

@Target(ElementType::TYPE)
@Active(typeof(NLSProcessor))
annotation NLS {
    String propertyFileName
}

class NLSProcessor extends AbstractClassProcessor {
    private static final String BUNDLE_NAME_FIELD = "BUNDLE_NAME"
    private static final String RESOURCE_BUNDLE_FIELD = "RESOURCE_BUNDLE"
    private static final String nlsClass = "org.eclipse.osgi.util.NLS"

    override doTransform(MutableClassDeclaration annotatedClass, extension TransformationContext context) {
        val nlsAnnotation = annotatedClass.getNLSAnnotation(context)
        if (findTypeGlobally(nlsClass) === null) {
            nlsAnnotation.addError(nlsClass + " isn't on the classpath.")
        }
        val propertyFileNameValue = nlsAnnotation.getNLSAnnotationPropertyValue(context)
        val propertiesFile = annotatedClass.getPropertiesFile(context, propertyFileNameValue, nlsAnnotation)
        if(propertiesFile !== null) {
            val propertiesFileInputStream = propertiesFile.getContentsAsStream
            val properties = propertiesFileInputStream.loadPropertiesFile(context, nlsAnnotation)
            addBundleNameField(annotatedClass, nlsAnnotation, context, propertyFileNameValue)
            addResourceBundleField(annotatedClass, nlsAnnotation, context)
            addStaticBlock(annotatedClass, nlsAnnotation, context)
            addGetStringMethod(annotatedClass, nlsAnnotation, context)
            properties.entrySet.sortBy[String.valueOf(key)].forEach [
                addField(annotatedClass, nlsAnnotation, context)
                addMethod(annotatedClass, nlsAnnotation, context)
            ]
        }
    }

    def private void addStaticBlock(MutableClassDeclaration annotatedClass, MutableAnnotationReference nlsAnnotation,
        extension TransformationContext context) {
        val fieldName = "INITIALIZER";
        checkForExistentField(annotatedClass, fieldName, context, nlsAnnotation)
        annotatedClass.addField(fieldName) [
            visibility = Visibility.PRIVATE
            static = true
            final = true
            type = string
            initializer = [
                '''
                    new «toJavaCode(Function0.newTypeReference(string))»() {
                        public «string» apply() {
                          «toJavaCode(nlsClass.newTypeReference)».initializeMessages(«annotatedClass.findDeclaredField(
                        BUNDLE_NAME_FIELD).simpleName», «toJavaCode(annotatedClass.qualifiedName.newTypeReference)».class);
                          return "";
                        }
                      }.apply();
                        '''
            ]
        ]
    }

    def private void addBundleNameField(MutableClassDeclaration annotatedClass, MutableAnnotationReference nlsAnnotation,
        extension TransformationContext context, String propertyFileName) {
        checkForExistentField(annotatedClass, BUNDLE_NAME_FIELD, context, nlsAnnotation)
        annotatedClass.addField(BUNDLE_NAME_FIELD) [
            visibility = Visibility.PRIVATE
            static = true
            final = true
            type = string
            initializer = [
                '''«toJavaCode(annotatedClass.qualifiedName.newTypeReference)».class.getPackage().getName() + ".«propertyFileName.
                    replace(".properties", "")»"''']
        ]
    }

    def private void addResourceBundleField(MutableClassDeclaration annotatedClass,
        MutableAnnotationReference nlsAnnotation, extension TransformationContext context) {
        checkForExistentField(annotatedClass, RESOURCE_BUNDLE_FIELD, context, nlsAnnotation)
        annotatedClass.addField(RESOURCE_BUNDLE_FIELD) [
            visibility = Visibility.PRIVATE
            static = true
            final = true
            type = ResourceBundle.newTypeReference
            initializer = [
                '''«toJavaCode(ResourceBundle.newTypeReference)».getBundle(«annotatedClass.
                    findDeclaredField(BUNDLE_NAME_FIELD).simpleName»)''']
        ]
    }

    def private void addGetStringMethod(MutableClassDeclaration annotatedClass, MutableAnnotationReference nlsAnnotation,
        extension TransformationContext context) {
        val methodName = "getString"
        checkForExistentMethod(annotatedClass, methodName, context, nlsAnnotation, 1)
        annotatedClass.addMethod(methodName) [
            visibility = Visibility.PRIVATE
            static = true
            returnType = string
            addParameter("key", string)
            body = [
                '''
                    try {
                        return «annotatedClass.findDeclaredField(RESOURCE_BUNDLE_FIELD).simpleName».getString(key);
                    } catch («toJavaCode(MissingResourceException.newTypeReference)» e) {
                        return '!' + key + '!';
                    }
                '''
            ]
        ]
    }

    def private void addField(Map.Entry<Object, Object> entry, MutableClassDeclaration annotatedClass,
        MutableAnnotationReference nlsAnnotation, extension TransformationContext context) {
        val fieldName = entry.key as String
        checkForExistentField(annotatedClass, fieldName, context, nlsAnnotation)
        annotatedClass.addField(fieldName) [
            visibility = Visibility.PUBLIC
            static = true
            final = true
            type = string
            initializer = ['''"«entry.key»"''']
        ]
    }

    def private void checkForExistentField(MutableClassDeclaration annotatedClass, String fieldName,
        extension TransformationContext context, MutableAnnotationReference nlsAnnotation) {
        if (annotatedClass.findDeclaredField(fieldName) !== null) {
            nlsAnnotation.addError("Field " + fieldName + " already present in class.")
        }
    }

    def private void addMethod(Map.Entry<Object, Object> entry, MutableClassDeclaration annotatedClass,
        MutableAnnotationReference nlsAnnotation, extension TransformationContext context) {
        val message = entry.value as String
        val wildcardCount = message.getWildcardCount
        val params = if (wildcardCount > 0) (0 .. wildcardCount - 1).map["param" + it] else newArrayList
        val getMessageForMethodName = "getMessageFor" + entry.key
        checkForExistentMethod(annotatedClass, getMessageForMethodName, context, nlsAnnotation, params.size)
        annotatedClass.addMethod(getMessageForMethodName) [
            visibility = Visibility.PUBLIC
            static = true
            returnType = context.string
            params.forEach(param|addParameter(param, object))
            body = [
                '''return «toJavaCode(nlsClass.newTypeReference)».bind(getString(«entry.key»), new Object [] { «params.
                    join(", ")» });'''
            ]
        ]
    }

    private def void checkForExistentMethod(MutableClassDeclaration annotatedClass, String methodName,
        extension TransformationContext context, MutableAnnotationReference nlsAnnotation, int parameterListSize) {
        val existentMethod = annotatedClass.findDeclaredMethod(methodName)
        if (existentMethod !== null) {
            if (existentMethod.parameters.size == parameterListSize) {
                nlsAnnotation.addError("Method " + methodName + "/" + parameterListSize + " already present in class.")
            }
        }
    }

    def private getWildcardCount(String unboundMessage) {
        val pattern = Pattern.compile("\\{\\d*\\}")
        val matcher = pattern.matcher(unboundMessage);
        val matches = <String>newHashSet
        while (matcher.find) {
            val matchResult = matcher.toMatchResult
            matches.add(matchResult.group())
        }
        matches.size
    }

    def private getNLSAnnotation(MutableClassDeclaration annotatedClass, extension TransformationContext context) {
        annotatedClass.findAnnotation(NLS.newTypeReference.type)
    }

    def private getNLSAnnotationPropertyValue(MutableAnnotationReference nlsAnnotation,
        extension TransformationContext context) {
        val value = nlsAnnotation.getValue('propertyFileName') as String
        if (value.nullOrEmpty) {
            nlsAnnotation.addError("@NLS requires non empty propertyFileName property value.")
        }
        value
    }

    def private getPropertiesFile(MutableClassDeclaration annotatedClass, extension TransformationContext context,
        String propertyFileName, MutableAnnotationReference nlsAnnotation) {
        val folder = annotatedClass.compilationUnit.filePath?.parent
        if (folder === null || !folder.exists) {
            nlsAnnotation.addError("Cannot find folder for class " + annotatedClass.qualifiedName + ": " + folder)
            throw new IllegalArgumentException(
                "Cannot find folder for class " + annotatedClass.qualifiedName + ": " + folder)
        }
        val propertiesFilePath = folder.append(
            propertyFileName + (if (!propertyFileName.endsWith(".properties")) ".properties" else ""))
        if (!propertiesFilePath.exists) {
            nlsAnnotation.addError(propertiesFilePath + " doesn't exist.")
        } else {
            propertiesFilePath
        }
    }

    def private loadPropertiesFile(InputStream propertiesFile, extension TransformationContext transformationContext,
        MutableAnnotationReference nlsAnnotation) {
        val properties = new Properties
        try {
            properties.load(propertiesFile)
        } catch (IOException ioe) {
            nlsAnnotation.addError("Cannot load properties file: " + ioe.message)
        }
        properties
    }
}
