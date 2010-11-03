package org.erlide.eunit.ui.launch;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.debug.ui.AbstractLaunchConfigurationTab;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.ui.dialogs.ElementListSelectionDialog;
import org.eclipse.ui.dialogs.SelectionDialog;
import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlProject;
import org.erlide.eunit.runtime.launch.IErlTestAttributes;
import org.erlide.eunit.runtime.launch.TestType;
import org.erlide.jinterface.util.ErlLogger;
import org.erlide.ui.util.SWTUtil;

/**
 * Main panel of EUnit run configuration
 * 
 * @author Aleksandra Lipiec
 *
 */
public class EUnitMainTab extends AbstractLaunchConfigurationTab {

	private ItemBrowser projectMBr; 	// projects browser
	private ItemBrowser moduleBr;	// module browser
	private ItemBrowser fileBr;		// file/directory browser
	private ItemBrowser projectAppBr;	// project browser for applications
	private ItemBrowser appBr;		// application browser
	private Button ifCover;				// checkbox about performing cover
	private Button singleRadio;			//radio button for single module
	private Button allRadio;			// radio button for running all tests
	private Button appRadio;			// radio button for application
	
	
	public void createControl(Composite parent) {
		Composite comp = new Composite(parent, SWT.NONE);
        setControl(comp);
        
        GridLayout mainLayout = new GridLayout();
        mainLayout.numColumns = 3;
        mainLayout.verticalSpacing = 15;
        mainLayout.horizontalSpacing = 20;
        mainLayout.marginHeight = 15;
        mainLayout.marginHeight = 15;
        comp.setLayout(mainLayout);
        
        
        
        GridData gData = new GridData();
        gData.horizontalAlignment = GridData.FILL;
        gData.horizontalSpan = 3;
       //gData.widthHint = 400;
        
        singleRadio = new Button(comp, SWT.RADIO);
        singleRadio.setText("Run tests for a single module");
        singleRadio.setLayoutData(gData);
        
        createModuleGroup(comp);
        
        allRadio = new Button(comp, SWT.RADIO);
        allRadio.setText("Run all tests in specific project, folder or file");
        allRadio.setLayoutData(gData);
        
        createAllTestsGroup(comp);
        
        appRadio = new Button(comp, SWT.RADIO);
        appRadio.setText("Run tests for an application");
        appRadio.setLayoutData(gData);
        
        createApplicationGroup(comp);
        
        ifCover = new Button(comp, SWT.CHECK);
        ifCover.setText("Prepare coverage report");
        ifCover.setLayoutData(gData);
        
        Collection<IErlProject> projects;
        try {
            projects = ErlangCore.getModel().getErlangProjects();
            final List<String> ps = new ArrayList<String>();
            for (final IErlProject p : projects) {
                ps.add(p.getName());
            }
        } catch (final ErlModelException e) {
            ErlLogger.warn(e);
        }
	}

	
	public void setDefaults(ILaunchConfigurationWorkingCopy config) {
		config.setAttribute(IErlTestAttributes.PROJECT, "");
        config.setAttribute(IErlTestAttributes.MODULE, "");
        config.setAttribute(IErlTestAttributes.FILE, "");
        config.setAttribute(IErlTestAttributes.APPLICATION, "");
        config.setAttribute(IErlTestAttributes.TYPE,
                TestType.MODULE.toString());
        config.setAttribute(IErlTestAttributes.COVER,
                Boolean.toString(true));
	}

	
	public void initializeFrom(ILaunchConfiguration config) {
		
		try {
			projectMBr.setText(config.getAttribute(
				IErlTestAttributes.PROJECT, ""));
		} catch (CoreException e) {
			projectMBr.setText("");
		}
		
		try {
			moduleBr.setText(config.getAttribute(
				IErlTestAttributes.MODULE, ""));
		} catch (CoreException e) {
			moduleBr.setText("");
		}
		
		try {
			fileBr.setText(config.getAttribute(
				IErlTestAttributes.FILE, ""));
		} catch (CoreException e) {
			fileBr.setText("");
		}
		
		try {
			projectAppBr.setText(config.getAttribute(
				IErlTestAttributes.APP_PROJECT, ""));
		} catch (CoreException e) {
			projectAppBr.setText("");
		}
		
		try {
			appBr.setText(config.getAttribute(
				IErlTestAttributes.APPLICATION, ""));
		} catch (CoreException e) {
			appBr.setText("");
		}
		
		try {
			String type = config.getAttribute(IErlTestAttributes.TYPE, 
				TestType.MODULE.toString());
			
			TestType typeT = TestType.valueOf(type);
			switch(typeT){
			case MODULE :	singleRadio.setSelection(true); break;
			case ALL	:	allRadio.setSelection(true); break;
			case APPLICATION	: appRadio.setSelection(true); break;
			default		: 	singleRadio.setSelection(true); break;
			}
		} catch (CoreException e) {
			singleRadio.setSelection(true);
		}
		
		try {
			String cover = config.getAttribute(IErlTestAttributes.COVER,
					Boolean.toString(true));
			ifCover.setSelection(Boolean.parseBoolean(cover));
		} catch (CoreException e) {
			ifCover.setSelection(true);
		}
				
	}

	
	public void performApply(ILaunchConfigurationWorkingCopy config) {
		
		//TODO: inform, when nothing sellected; is it possible?
		TestType type;
		if(allRadio.getSelection()) {
			type = TestType.ALL;
		} else if(appRadio.getSelection()) {
			type = TestType.APPLICATION;
		} else {
			type = TestType.MODULE;
		}
		
		boolean cover = ifCover.getSelection();
		
		config.setAttribute(IErlTestAttributes.PROJECT, projectMBr.getText());
        config.setAttribute(IErlTestAttributes.MODULE, moduleBr.getText());
        config.setAttribute(IErlTestAttributes.FILE, fileBr.getText());
        config.setAttribute(IErlTestAttributes.APPLICATION, appBr.getText());
        config.setAttribute(IErlTestAttributes.TYPE, type.toString());
        config.setAttribute(IErlTestAttributes.COVER,
                Boolean.toString(cover));
	}

	
	public String getName() {
		return "EUnit";
	}
	
	private void createModuleGroup(final Composite comp){
		
		ElementListSelectionDialog dialogProject = 
			new ElementListSelectionDialog(this.getShell(), 
					new LabelProvider());
		
		ElementListSelectionDialog dialogModule =
			new ElementListSelectionDialog(this.getShell(),
					new LabelProvider());
				
		
		projectMBr = browserWithLabel(comp, "Project:",
				dialogProject);
		moduleBr = browserWithLabel(comp, "Module:",
				dialogModule);
		
	}
	
	private void createAllTestsGroup(final Composite comp){
        
        ElementListSelectionDialog dialogFile = 
			new ElementListSelectionDialog(this.getShell(), 
					new LabelProvider());
        
        fileBr = new ItemBrowser(comp, SWT.SINGLE | SWT.BORDER, dialogFile);
        fileBr.setFiledLength(600);
        fileBr.getTextGridData().horizontalSpan = 2;
		
	}
	
	private void createApplicationGroup(final Composite comp){
		
		ElementListSelectionDialog dialogProject = 
			new ElementListSelectionDialog(this.getShell(), 
					new LabelProvider());
		
		ElementListSelectionDialog dialogApp =
			new ElementListSelectionDialog(this.getShell(),
					new LabelProvider());
				
		
		projectAppBr = browserWithLabel(comp, "Project:",
				dialogProject);
		appBr = browserWithLabel(comp, "Module:",
				dialogApp);
		
	}
	
	private ItemBrowser browserWithLabel(Composite comp,
			String text, SelectionDialog dialog){
	    
		GridData gData = new GridData();
		//gData.widthHint = 50;
		//gData.horizontalSpan = 1;
		
		Label label = new Label(comp, SWT.NONE);
		label.setLayoutData(gData);
		label.setText(text);
		
		ItemBrowser browser = new ItemBrowser(
				comp, SWT.SINGLE | SWT.BORDER, dialog);
		//browser.addModifyListener(ml);
		
	    return browser;
	}

}
