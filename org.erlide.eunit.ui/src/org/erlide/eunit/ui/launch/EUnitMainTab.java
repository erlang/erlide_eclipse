package org.erlide.eunit.ui.launch;

import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.debug.ui.AbstractLaunchConfigurationTab;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerFilter;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.dialogs.ElementListSelectionDialog;
import org.eclipse.ui.dialogs.ElementTreeSelectionDialog;
import org.eclipse.ui.dialogs.SelectionDialog;
import org.eclipse.ui.ide.IDE;
import org.eclipse.ui.model.BaseWorkbenchContentProvider;
import org.eclipse.ui.model.WorkbenchLabelProvider;
import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.IErlProject;
import org.erlide.eunit.runtime.launch.IErlTestAttributes;
import org.erlide.eunit.runtime.launch.TestType;
import org.erlide.eunit.ui.launch.helpers.ProjectElement;
import org.erlide.eunit.ui.launch.helpers.ProjectLabelProvider;
import org.erlide.jinterface.util.ErlLogger;
import org.erlide.ui.editors.erl.outline.ErlangElementImageProvider;

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
	
	private ElementListSelectionDialog moduleDialog;
	
	
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
        singleRadio.addSelectionListener(radioSelectionListener);
        
        createModuleGroup(comp);
        
        allRadio = new Button(comp, SWT.RADIO);
        allRadio.setText("Run all tests in specific project, folder or file");
        allRadio.setLayoutData(gData);
        allRadio.addSelectionListener(radioSelectionListener);
        
        createAllTestsGroup(comp);
        
        appRadio = new Button(comp, SWT.RADIO);
        appRadio.setText("Run tests for an application");
        appRadio.setLayoutData(gData);
        appRadio.addSelectionListener(radioSelectionListener);
        
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
		    
		    String projectName = config.getAttribute(
	                IErlTestAttributes.PROJECT, "");
		    
			projectMBr.setText(projectName);
			
			if(projectName != null && projectName.length() > 0) {
			    IErlProject p = ErlangCore.getModel().
			        getErlangProject(projectName);
			    if( p != null)
			        moduleDialog.setElements(createModuleArray(p));
			}
			
			
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
			case MODULE :	
			    
			    singleRadio.setSelection(true);
			    
			    projectMBr.setEnabled(true);
                moduleBr.setEnabled(true);
                fileBr.setEnabled(false);
                projectAppBr.setEnabled(false);
                appBr.setEnabled(false);
                
			    break;
			    
			case ALL	:	
			    
			    allRadio.setSelection(true); 
			    
			    projectMBr.setEnabled(false);
                moduleBr.setEnabled(false);
                fileBr.setEnabled(true);
                projectAppBr.setEnabled(false);
                appBr.setEnabled(false);
			    
                break;
                
			case APPLICATION	: 
			    
			    appRadio.setSelection(true); 
			    
			    projectMBr.setEnabled(false);
                moduleBr.setEnabled(false);
                fileBr.setEnabled(false);
                projectAppBr.setEnabled(true);
                appBr.setEnabled(true);
			    
			    break;
			    
			default		: 	
			    
			    singleRadio.setSelection(true); 
			    
			    projectMBr.setEnabled(true);
                moduleBr.setEnabled(true);
                fileBr.setEnabled(false);
                projectAppBr.setEnabled(false);
                appBr.setEnabled(false);
			    
			    break;
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
	
	private void createModuleGroup(final Composite comp) {
        
		ElementListSelectionDialog projectDialog = 
			new ElementListSelectionDialog(this.getShell(), 
					new ProjectLabelProvider());
		
		Object[] elements = createProjectArray();
		
		projectDialog.setElements(elements);
		projectDialog.setTitle("Select project");
		projectDialog.setMessage("Select Erlang project: ");
		
		moduleDialog = new ElementListSelectionDialog(this.getShell(),
					new ProjectLabelProvider());
				
		moduleDialog.setElements(new Object[0]);
		moduleDialog.setTitle("Select module");
		moduleDialog.setMessage("Select Erlang module: ");
		
		projectMBr = browserWithLabel(comp, "Project:",
				projectDialog);
		projectMBr.addModifyListener( new ModifyListener() {

            public void modifyText(ModifyEvent e) {
                updateLaunchConfigurationDialog();
                String projectName = projectMBr.getText();
                if(projectName != null && projectName.length() > 0){
                    IErlProject p = ErlangCore.getModel().
                        getErlangProject(projectName);
                    if( p != null)
                        moduleDialog.setElements(createModuleArray(p));
                }
                    
            }
		    
		});		
		
		moduleBr = browserWithLabel(comp, "Module:",
				moduleDialog);
		moduleBr.addModifyListener(basicModifyListener);
		
	}
	
	private Object[] createProjectArray() {
	    Object[] array;
        try {
            List<ProjectElement> res = new LinkedList<ProjectElement>();

            Collection<IErlProject> projects = 
                ErlangCore.getModel().getErlangProjects();
            
            for(IErlProject p : projects){
                ProjectElement elem = new ProjectElement(p.getName(),
                        PlatformUI.getWorkbench().getSharedImages()
                        .getImageDescriptor(
                                IDE.SharedImages.IMG_OBJ_PROJECT).createImage());
                
                res.add(elem);
            }
            array = res.toArray();
            
        } catch (ErlModelException e) {
            array = new Object[0];
            e.printStackTrace();
        }
        return array;
	}
	
	private Object[] createModuleArray(IErlProject p) {
	    Object[] array;
        try {
            List<ProjectElement> res = new LinkedList<ProjectElement>();

            Collection<IErlModule> modules = p.getModules();
            
            for(IErlModule m : modules) {
                ProjectElement elem = new ProjectElement(m.getName(),
                        ErlangElementImageProvider
                        .getErlImageDescriptor(m,
                                ErlangElementImageProvider.SMALL_ICONS).
                                createImage());
                res.add(elem);
            }
            array = res.toArray();
            
        } catch (ErlModelException e) {
            array = new Object[0];
            e.printStackTrace();
        }
        return array;
	}
	
	private void createAllTestsGroup(final Composite comp){
        
    //    ElementListSelectionDialog dialogFile = 
	//		new ElementListSelectionDialog(this.getShell(), 
	//				new LabelProvider());
        
        ElementTreeSelectionDialog fileDialog = new ElementTreeSelectionDialog(
                this.getShell(),
                new WorkbenchLabelProvider(),
                new BaseWorkbenchContentProvider());
        fileDialog.setTitle("Select file ore directory");
        fileDialog.setMessage("Select project, directory or file: ");
        try {
            fileDialog.setInput(ErlangCore.getModel().getErlangProjects());
        } catch (ErlModelException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        
        
     /*   fileDialog.addFilter(new ViewerFilter() {

            @Override
            public boolean select(Viewer viewer, Object parentElement,
                    Object element) {
                
                IWorkspaceRoot workspaceRoot = 
                    ResourcesPlugin.getWorkspace().getRoot();
                
                if(parentElement.equals(workspaceRoot) && 
                        element instanceof IProject){
                    String name = ((IProject)element).getName();
                    if(ErlangCore.getModel().getErlangProject(name) != null) {
                        return true;
                    }
                }
                    
                return false;
            }
            
        });*/
        
        fileBr = new ItemBrowser(comp, SWT.SINGLE | SWT.BORDER, fileDialog);
        fileBr.setFiledLength(600);
        fileBr.getTextGridData().horizontalSpan = 2;
        fileBr.addModifyListener(basicModifyListener);		
	}
	
	private void createApplicationGroup(final Composite comp){
		
		ElementListSelectionDialog projectDialog = 
			new ElementListSelectionDialog(this.getShell(), 
					new LabelProvider());
		
		Object[] elements = createProjectArray();
		
		projectDialog.setElements(elements);
        projectDialog.setTitle("Select project");
        projectDialog.setMessage("Select Erlang project: ");
		
		ElementListSelectionDialog appDialog =
			new ElementListSelectionDialog(this.getShell(),
					new LabelProvider());
		
		//TODO: create model for appilcations
		appDialog.setElements(new Object[0]);
		appDialog.setTitle("Select application");
		appDialog.setMessage("Select Erlang application: ");
		
		projectAppBr = browserWithLabel(comp, "Project:",
				projectDialog);
		projectAppBr.addModifyListener(basicModifyListener);
		
		appBr = browserWithLabel(comp, "Module:",
				appDialog);
		appBr.addModifyListener(basicModifyListener);
		
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
	
	private final ModifyListener basicModifyListener = new ModifyListener() {
        @SuppressWarnings("synthetic-access")
        public void modifyText(final ModifyEvent evt) {
            updateLaunchConfigurationDialog();
        }
    };
    
    private final SelectionListener radioSelectionListener =
        new SelectionListener() {

        public void widgetSelected(SelectionEvent e) {
        
            if(e.widget.equals(singleRadio)){
                projectMBr.setEnabled(true);
                moduleBr.setEnabled(true);
                fileBr.setEnabled(false);
                projectAppBr.setEnabled(false);
                appBr.setEnabled(false);
            } else if (e.widget.equals(allRadio)) {
                projectMBr.setEnabled(false);
                moduleBr.setEnabled(false);
                fileBr.setEnabled(true);
                projectAppBr.setEnabled(false);
                appBr.setEnabled(false);
            } else if (e.widget.equals(appRadio)) {
                projectMBr.setEnabled(false);
                moduleBr.setEnabled(false);
                fileBr.setEnabled(false);
                projectAppBr.setEnabled(true);
                appBr.setEnabled(true);
            }
        }

        public void widgetDefaultSelected(SelectionEvent e) {
            // TODO Auto-generated method stub
            
        }
        
    };

}
