describe('Create Patient Profile', () => {
  
  beforeEach(() => {
    cy.intercept('POST', '/Patient/Create-PatientProfile', {
      statusCode: 200,
      body: {
        message: 'Patient profile successfully created.'
      }
    }).as('createPatientProfile');

    cy.visit('http://localhost:4200/login');
        cy.get('input[name="email"]').type("matildexv.04@gmail.com");
        cy.get('input[name="password"]').type("Abcde12345!");
        cy.get('button.add-button-submit').click();
        cy.wait(3000);
        cy.visit('http://localhost:4200/create-patient-profile');
  });

  it('should load the correct title and input fields', () => {
    cy.get('h1.main-title').should('contain', 'Create Patient Profile');
    cy.get('input[name="firstName"]').should('exist');
    cy.get('input[name="lastName"]').should('exist');
    cy.get('input[name="phone"]').should('exist');
    cy.get('input[name="emergencyContact"]').should('exist');
    cy.get('input[name="email"]').should('exist');
    cy.get('input[name="address"]').should('exist');
    cy.get('input[name="gender"]').should('exist');
    cy.get('input[name="dateBirth"]').should('exist');
    cy.get('button.add-button-submit').should('exist');
    cy.get('button.add-button-clear').should('exist');
  });


  it('should enable the submit button when the form is valid', () => {

    cy.get('input[name="firstName"]').type('John');
    cy.get('input[name="lastName"]').type('Doe');
    cy.get('input[name="phone"]').type('+351 910001234');
    cy.get('input[name="emergencyContact"]').type('+351 9100081234');
    cy.get('input[name="email"]').type('john.doe@example.com');
    cy.get('input[name="address"]').type('Portugal, 4590-445, Rua da Sardinha');
    cy.get('input[name="gender"]').type('Male');
    cy.get('input[name="dateBirth"]').type('20/10/2004');

    cy.get('button.add-button-submit').should('not.be.disabled');
  });

  it('should submit the form with valid data', () => {

    cy.get('input[name="firstName"]').type('John');
    cy.get('input[name="lastName"]').type('Doe');
    cy.get('input[name="phone"]').type('+351 910001234');
    cy.get('input[name="emergencyContact"]').type('+351 9100081234');
    cy.get('input[name="email"]').type('john.doe@example.com');
    cy.get('input[name="address"]').type('Portugal, 4590-445, Rua da Sardinha');
    cy.get('input[name="gender"]').type('Male');
    cy.get('input[name="dateBirth"]').type('20/10/2004');

    cy.get('button.add-button-submit').should('not.be.disabled');
    
    cy.get('button.add-button-submit').click();

    cy.wait(5000)

    cy.get('app-message').should('contain', 'Patient profile: john.doe@example.com was successfully created.');
  });

  it('should clear the form when the "Clear Input" button is clicked', () => {
    cy.get('input[name="firstName"]').type('John');
    cy.get('input[name="lastName"]').type('Doe');
    cy.get('input[name="phone"]').type('+351 910001234');
    cy.get('input[name="emergencyContact"]').type('+351 9100081234');
    cy.get('input[name="email"]').type('john.doe@example.com');
    cy.get('input[name="address"]').type('Portugal, 4590-445, Rua da Sardinha');
    cy.get('input[name="gender"]').type('Male');
    cy.get('input[name="dateBirth"]').type('20/10/2004');

    cy.get('button.add-button-clear').click();

    cy.get('input[name="firstName"]').should('have.value', '');
    cy.get('input[name="lastName"]').should('have.value', '');
    cy.get('input[name="phone"]').should('have.value', '');
    cy.get('input[name="emergencyContact"]').should('have.value', '');
    cy.get('input[name="email"]').should('have.value', '');
    cy.get('input[name="address"]').should('have.value', '');
    cy.get('input[name="gender"]').should('have.value', '');
    cy.get('input[name="dateBirth"]').should('have.value', '');
  });

  it('should disable form input fields after submission', () => {
    cy.get('input[name="firstName"]').type('John');
    cy.get('input[name="lastName"]').type('Doe');
    cy.get('input[name="phone"]').type('+351 910051234');
    cy.get('input[name="emergencyContact"]').type('+351 9100581234');
    cy.get('input[name="email"]').type('john.doe2@example.com');
    cy.get('input[name="address"]').type('Portugal, 4590-445, Rua da Sardinha');
    cy.get('input[name="gender"]').type('Male');
    cy.get('input[name="dateBirth"]').type('20/09/2004');

    cy.get('button.add-button-submit').click();

    cy.wait(5000);

    cy.get('input[name="firstName"]').should('be.disabled');
    cy.get('input[name="lastName"]').should('be.disabled');
    cy.get('input[name="phone"]').should('be.disabled');
    cy.get('input[name="emergencyContact"]').should('be.disabled');
    cy.get('input[name="email"]').should('be.disabled');
    cy.get('input[name="address"]').should('be.disabled');
    cy.get('input[name="gender"]').should('be.disabled');
    cy.get('input[name="dateBirth"]').should('be.disabled');
  });
});
