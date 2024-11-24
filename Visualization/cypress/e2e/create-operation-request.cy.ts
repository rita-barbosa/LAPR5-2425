describe('Create Operation Request', () => {
    const goodOperationRequest1 = {
      deadLineDate: '2024-12-31',
      priority: 'Emergency',
      dateOfRequest: '2024-11-23',
      status: 'Requested',
      staffId: 'D202400001',
      description: 'Test operation request',
      patientId: '202411000002',
      operationTypeId: 'Open ACL Reconstruction Surgery 2'
    };

    const goodOperationRequest2 = {
      deadLineDate: '2024-12-30',
      priority: 'Emergency',
      dateOfRequest: '2024-11-29',
      status: 'Requested',
      staffId: 'D202400001',
      description: 'Test operation request 2',
      patientId: '202411000002',
      operationTypeId: 'Open ACL Reconstruction Surgery 2'
    };

    const goodOperationRequest3 = {
      deadLineDate: '2024-12-31',
      priority: 'Emergency',
      dateOfRequest: '2024-11-10',
      status: 'Requested',
      staffId: 'D202400001',
      description: 'Test operation request 3',
      patientId: '202411000002',
      operationTypeId: 'Open ACL Reconstruction Surgery 2'
    };
  
    const successMessage = 'Operation Request successfully created';
    const errorMessage = 'Failed to create Operation Request!';
  
     beforeEach(() => {
        cy.visit('http://localhost:4200/login');
        cy.get('input[name="email"]').type("1220738@isep.ipp.pt");
        cy.get('input[name="password"]').type("Newpass123!");
        cy.get('button.add-button-submit').click();
        cy.wait(3000);
        cy.visit('http://localhost:4200/create-operation-request');
    }); 
  
     it('should create a valid operation request', () => {
        
        // Fill out the form with valid data
        cy.get('input[name="deadLineDate"]').type(goodOperationRequest1.deadLineDate);
        cy.get('input[name="priority"]').type(goodOperationRequest1.priority);
        cy.get('input[name="dateOfRequest"]').type(goodOperationRequest1.dateOfRequest);
        cy.get('input[name="status"]').type(goodOperationRequest1.status);
        cy.get('input[name="staffId"]').type(goodOperationRequest1.staffId);
        cy.get('input[name="description"]').type(goodOperationRequest1.description);
        cy.get('input[name="patientId"]').type(goodOperationRequest1.patientId);
        cy.get('select[name="operationTypeId"]').select(goodOperationRequest1.operationTypeId);
      
        // Submit the form
        cy.get('button.add-button-submit').click();
      
        // Wait for the request and verify the response
        cy.wait(5000)
        // Verify success message is shown on the page
        cy.contains(successMessage).should('be.visible');
      });

      it('should show Add to Patient form after successful creation', () => {
        // Submit the form with valid data
        cy.get('input[name="deadLineDate"]').type(goodOperationRequest2.deadLineDate);
        cy.get('input[name="priority"]').type(goodOperationRequest2.priority);
        cy.get('input[name="dateOfRequest"]').type(goodOperationRequest2.dateOfRequest);
        cy.get('input[name="status"]').type(goodOperationRequest2.status);
        cy.get('input[name="staffId"]').type(goodOperationRequest2.staffId);
        cy.get('input[name="description"]').type(goodOperationRequest2.description);
        cy.get('input[name="patientId"]').type(goodOperationRequest2.patientId);
        cy.get('select[name="operationTypeId"]').select(goodOperationRequest2.operationTypeId);
    
        // Submit the form
        cy.get('button.add-button-submit').click();
    
        // Verify Add to Patient button is shown
        cy.contains("Add to Patient's Medical History").should('be.visible');
  
        cy.get('button.add-button-patient').click();
  
        cy.contains("Click on the button to submit to add the operation request created to the patient medical history!").should('be.visible');
  
        cy.get('button.add-button-add-patient').click();
  
        cy.contains("Operation request successfully add to patient history.").should('be.visible');
      });

      it('should return error creating an invalid operation request (Deadline date)', () => {
        
        // Fill out the form with valid data
        cy.get('input[name="deadLineDate"]').type("234-43-5352");
        cy.get('input[name="priority"]').type(goodOperationRequest1.priority);
        cy.get('input[name="dateOfRequest"]').type(goodOperationRequest1.dateOfRequest);
        cy.get('input[name="status"]').type(goodOperationRequest1.status);
        cy.get('input[name="staffId"]').type(goodOperationRequest1.staffId);
        cy.get('input[name="description"]').type(goodOperationRequest1.description);
        cy.get('input[name="patientId"]').type(goodOperationRequest1.patientId);
        cy.get('select[name="operationTypeId"]').select(goodOperationRequest1.operationTypeId);
      
        // Submit the form
        cy.get('button.add-button-submit').click();
      
        // Wait for the request and verify the response
        cy.wait(2000)
        // Verify success message is shown on the page
        cy.contains(errorMessage).should('be.visible');
      });

      it('should return error creating an invalid operation request (Priority)', () => {
        
        // Fill out the form with valid data
        cy.get('input[name="deadLineDate"]').type(goodOperationRequest1.deadLineDate);
        cy.get('input[name="priority"]').type("Fast and Furious");
        cy.get('input[name="dateOfRequest"]').type(goodOperationRequest1.dateOfRequest);
        cy.get('input[name="status"]').type(goodOperationRequest1.status);
        cy.get('input[name="staffId"]').type(goodOperationRequest1.staffId);
        cy.get('input[name="description"]').type(goodOperationRequest1.description);
        cy.get('input[name="patientId"]').type(goodOperationRequest1.patientId);
        cy.get('select[name="operationTypeId"]').select(goodOperationRequest1.operationTypeId);
      
        // Submit the form
        cy.get('button.add-button-submit').click();
      
        // Wait for the request and verify the response
        cy.wait(2000)
        // Verify success message is shown on the page
        cy.contains(errorMessage).should('be.visible');
      });

      it('should return error creating an invalid operation request (Date of Request)', () => {
        
        // Fill out the form with valid data
        cy.get('input[name="deadLineDate"]').type(goodOperationRequest1.deadLineDate);
        cy.get('input[name="priority"]').type(goodOperationRequest1.priority);
        cy.get('input[name="dateOfRequest"]').type("234-43-5352");
        cy.get('input[name="status"]').type(goodOperationRequest1.status);
        cy.get('input[name="staffId"]').type(goodOperationRequest1.staffId);
        cy.get('input[name="description"]').type(goodOperationRequest1.description);
        cy.get('input[name="patientId"]').type(goodOperationRequest1.patientId);
        cy.get('select[name="operationTypeId"]').select(goodOperationRequest1.operationTypeId);
      
        // Submit the form
        cy.get('button.add-button-submit').click();
      
        // Wait for the request and verify the response
        cy.wait(2000)
        // Verify success message is shown on the page
        cy.contains(errorMessage).should('be.visible');
      });

      it('should return error creating an invalid operation request (StaffId)', () => {
        
        // Fill out the form with valid data
        cy.get('input[name="deadLineDate"]').type(goodOperationRequest1.deadLineDate);
        cy.get('input[name="priority"]').type(goodOperationRequest1.priority);
        cy.get('input[name="dateOfRequest"]').type(goodOperationRequest1.dateOfRequest);
        cy.get('input[name="status"]').type(goodOperationRequest1.status);
        cy.get('input[name="staffId"]').type("753f307853");
        cy.get('input[name="description"]').type(goodOperationRequest1.description);
        cy.get('input[name="patientId"]').type(goodOperationRequest1.patientId);
        cy.get('select[name="operationTypeId"]').select(goodOperationRequest1.operationTypeId);
      
        // Submit the form
        cy.get('button.add-button-submit').click();
      
        // Wait for the request and verify the response
        cy.wait(2000)
        // Verify success message is shown on the page
        cy.contains(errorMessage).should('be.visible');
      });

      it('should return error creating an invalid operation request (PatientId)', () => {
        
        // Fill out the form with valid data
        cy.get('input[name="deadLineDate"]').type(goodOperationRequest1.deadLineDate);
        cy.get('input[name="priority"]').type(goodOperationRequest1.priority);
        cy.get('input[name="dateOfRequest"]').type(goodOperationRequest1.dateOfRequest);
        cy.get('input[name="status"]').type(goodOperationRequest1.status);
        cy.get('input[name="staffId"]').type(goodOperationRequest1.staffId);
        cy.get('input[name="description"]').type(goodOperationRequest1.description);
        cy.get('input[name="patientId"]').type("753f307853");
        cy.get('select[name="operationTypeId"]').select(goodOperationRequest1.operationTypeId);
      
        // Submit the form
        cy.get('button.add-button-submit').click();
      
        // Wait for the request and verify the response
        cy.wait(2000)
        // Verify success message is shown on the page
        cy.contains(errorMessage).should('be.visible');
      });

      it('should return error creating an invalid operation request (OperationRequestName)', () => {
        
        // Fill out the form with valid data
        cy.get('input[name="deadLineDate"]').type(goodOperationRequest1.deadLineDate);
        cy.get('input[name="priority"]').type(goodOperationRequest1.priority);
        cy.get('input[name="dateOfRequest"]').type(goodOperationRequest1.dateOfRequest);
        cy.get('input[name="status"]').type(goodOperationRequest1.status);
        cy.get('input[name="staffId"]').type(goodOperationRequest1.staffId);
        cy.get('input[name="description"]').type(goodOperationRequest1.description);
        cy.get('input[name="patientId"]').type("753f307853");
      
        // Submit the form
        cy.get('button.add-button-submit').should('be.disabled');
      
        // Wait for the request and verify the response
      });
  
    it('should reset form after clicking clear button', () => {
      // Fill the form with some values
      cy.get('input[name="deadLineDate"]').type('2024-12-31');
      cy.get('input[name="priority"]').type('Emergency');
      
      // Click on the clear button
      cy.get('.add-button-clear').click();
  
      // Verify the form is cleared
      cy.get('input[name="deadLineDate"]').should('have.value', '');
      cy.get('input[name="priority"]').should('have.value', '');
    });
  
    it('should disable submit button while the form is being submitted', () => {
      // Fill the form with valid data
      cy.get('input[name="deadLineDate"]').type(goodOperationRequest2.deadLineDate);
      cy.get('input[name="priority"]').type(goodOperationRequest2.priority);
      cy.get('input[name="dateOfRequest"]').type(goodOperationRequest2.dateOfRequest);
      cy.get('input[name="status"]').type(goodOperationRequest2.status);
      cy.get('input[name="staffId"]').type(goodOperationRequest2.staffId);
      cy.get('input[name="description"]').type(goodOperationRequest2.description);
      cy.get('input[name="patientId"]').type(goodOperationRequest2.patientId);
      cy.get('select[name="operationTypeId"]').select(goodOperationRequest2.operationTypeId);
  
      // Submit the form and verify the submit button is disabled
      cy.get('button.add-button-submit').should('be.disabled');
    });

    it('should not create an invalid operation request (Deadline Date < Date of Request)', () => {
        
      // Fill out the form with valid data
      cy.get('input[name="deadLineDate"]').type(goodOperationRequest1.dateOfRequest);
      cy.get('input[name="priority"]').type(goodOperationRequest1.priority);
      cy.get('input[name="dateOfRequest"]').type(goodOperationRequest1.deadLineDate);
      cy.get('input[name="status"]').type(goodOperationRequest1.status);
      cy.get('input[name="staffId"]').type(goodOperationRequest1.staffId);
      cy.get('input[name="description"]').type(goodOperationRequest1.description);
      cy.get('input[name="patientId"]').type(goodOperationRequest1.patientId);
      cy.get('select[name="operationTypeId"]').select(goodOperationRequest1.operationTypeId);
    
      // Submit the form
      cy.get('button.add-button-submit').click();
    
      // Wait for the request and verify the response
      cy.wait(5000)
      // Verify success message is shown on the page
      cy.contains(errorMessage).should('be.visible');
    });
  
    it('should disable form fields after submission', () => {
      // Fill out and submit the form
      cy.get('input[name="deadLineDate"]').type(goodOperationRequest3.deadLineDate);
      cy.get('input[name="priority"]').type(goodOperationRequest3.priority);
      cy.get('input[name="dateOfRequest"]').type(goodOperationRequest3.dateOfRequest);
      cy.get('input[name="status"]').type(goodOperationRequest3.status);
      cy.get('input[name="staffId"]').type(goodOperationRequest3.staffId);
      cy.get('input[name="description"]').type(goodOperationRequest3.description);
      cy.get('input[name="patientId"]').type(goodOperationRequest3.patientId);
      cy.get('select[name="operationTypeId"]').select(goodOperationRequest3.operationTypeId);
  
      // Submit the form
      cy.get('button.add-button-submit').click();
  
      // Verify the fields are disabled after submission
      cy.get('input[name="deadLineDate"]').should('be.disabled');
      cy.get('input[name="priority"]').should('be.disabled');
      cy.get('input[name="dateOfRequest"]').should('be.disabled');
      cy.get('input[name="status"]').should('be.disabled');
      cy.get('input[name="staffId"]').should('be.disabled');
      cy.get('input[name="description"]').should('be.disabled');
      cy.get('input[name="patientId"]').should('be.disabled');
      cy.get('select[name="operationTypeId"]').should('be.disabled');
    }); 
  });
  