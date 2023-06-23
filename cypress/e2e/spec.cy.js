describe("Regular Admin journey", () => {
  it("should see intro step", () => {
    cy.visit("http://localhost:8000/");
    // Intro Step
    cy.get('[data-testid="intro-h1"]').should(
      "have.text",
      "Hi ! Welcome to EST Poker !"
    );
    cy.get('[data-testid="intro-sub-head"]').should(
      "have.text",
      "[ Simple web app for estimation story points within team ]"
    );
    cy.get('[data-testid="intro-punchline"]').should(
      "have.text",
      "Precise Planning, Efficient Execution, Blazing Fast !"
    );
    cy.get('[data-testid="intro-submit"]').click();
  });
  it("should get error on the admin step", () => {
    cy.get('[data-testid="enter-name-admin-text"]').should(
      "have.text",
      "Add your name"
    );
    cy.get('[data-testid="enter-name-admin-info-text"]').should(
      "have.text",
      "[ You're about to become an admin ]"
    );

    cy.get('[data-testid="enter-name-admin-submit"]').click();
    // Error assertion
    cy.get('[data-testid="error-message"]').should(
      "contain.text",
      "Input is empty"
    );
    cy.get('[data-testid="enter-name-admin-input"]').should(
      "have.css",
      "border-color",
      "rgb(239, 68, 68)"
    );
  });
  it("should pass admin step", () => {
    cy.get('[data-testid="enter-name-admin-input"]').type("Steve");
    cy.get('[data-testid="enter-name-admin-submit"]').click();
    cy.get('[data-testid="error-message"]').should(
      "have.css",
      "display",
      "none"
    );
  });
  it("should get error on room step", () => {
    cy.get('[data-testid="enter-room-text"]').should(
      "have.text",
      "Create new room"
    );
    cy.get('[data-testid="enter-room-info-text"]').should(
      "have.text",
      "[ Place where you can vote for stories ]"
    );

    cy.get('[data-testid="enter-room-submit"]').click();
    // Error assertion
    cy.get('[data-testid="error-message"]').should(
      "contain.text",
      "Input is empty"
    );
    cy.get('[data-testid="enter-room-input"]').should(
      "have.css",
      "border-color",
      "rgb(239, 68, 68)"
    );
  });
  it("should pass room step", () => {
    cy.get('[data-testid="enter-room-input"]').type(
      "Most Agile Team out there !"
    );
    cy.get('[data-testid="enter-room-submit"]').click();
    cy.get('[data-testid="error-message"]').should(
      "have.css",
      "display",
      "none"
    );
  });
  it("should get error on story step by trying to add new one or save", () => {
    cy.get('[data-testid="create-story-text"]').should(
      "have.text",
      "Create new story"
    );
    cy.get('[data-testid="create-story-info-text"]').should(
      "have.text",
      "[ Add multiple or one story ]"
    );
    // Error on Add New
    cy.get('[data-testid="create-story-add"]').click();
    cy.get('[data-testid="error-message"]').should(
      "have.css",
      "display",
      "none"
    );
    // Error on Save
    cy.get('[data-testid="create-story-add"]').click();
    cy.get('[data-testid="error-message"]').should(
      "have.css",
      "display",
      "none"
    );
  });
  it("should be able to add multiple stories and stay on same step", () => {
    cy.get('[data-testid="create-story-input"]').type(
      "FGTH-1234: Technical debt jira"
    );
    cy.get('[data-testid="create-story-add"]').click();
    cy.get('[data-testid="create-story-input"]').type("FGTH-4321: Refactoring");
    cy.get('[data-testid="create-story-add"]').click();
    cy.get('[data-testid="create-story-input"]').type(
      "FGTH-4545: One more story"
    );
    cy.get('[data-testid="create-story-add"]').click();
    // Make sure button Save is still there - we know we are on the same step
    cy.get('[data-testid="create-story-submit"]').should("have.text", "Save");
  });
  it("should be able to save stories and continue", () => {
    cy.get('[data-testid="create-story-input"]').type(
      "FGTH-7777: Last story for the sprint"
    );
    cy.get('[data-testid="create-story-submit"]').click();
  });
  // it("should choose common sequence", () => {
  //   cy.visit("https://example.cypress.io");
  // });
  // it("should disabled opposite section by clicking on checkbox", () => {
  //   cy.visit("https://example.cypress.io");
  // });
  // it("should type and send custom sequence", () => {
  //   cy.visit("https://example.cypress.io");
  // });
  // it("should't be able to type unsupported characters", () => {
  //   cy.visit("https://example.cypress.io");
  // });
  // it("should be able to see all story point fields that we had on SequenceStep", () => {
  //   cy.visit("https://example.cypress.io");
  // });
  // it("should produce notification when click 'Copy URL' button", () => {
  //   cy.visit("https://example.cypress.io");
  // });
  // it("should kick-off timer when click on 'Start timer'", () => {
  //   cy.visit("https://example.cypress.io");
  // });
  // it("should kick-off timer and get notification when click on 'Start timer'", () => {
  //   cy.visit("https://example.cypress.io");
  // });
  // it("should be able to preserve same room name when try to edit room name and just save", () => {
  //   cy.visit("https://example.cypress.io");
  // });
  // it("should be able to change room name when try to edit room name", () => {
  //   cy.visit("https://example.cypress.io");
  // });
  // it("should be able to add new story", () => {
  //   cy.visit("https://example.cypress.io");
  // });
  // it("should edit story by typing new value in input", () => {
  //   cy.visit("https://example.cypress.io");
  // });
  // it("should preserve old story state when save edited story with empty value", () => {
  //   cy.visit("https://example.cypress.io");
  // });
  // it("should make edited story is in the right spot in stories list", () => {
  //   cy.visit("https://example.cypress.io");
  // });
  // it("should get other buttons when click on 'Start timer'", () => {
  //   cy.visit("https://example.cypress.io");
  // });
  // it("should be able to vote and it's reflected in Team: list", () => {
  //   cy.visit("https://example.cypress.io");
  // });
  // it("should see new buttons when vote is choosen", () => {
  //   cy.visit("https://example.cypress.io");
  // });
  // it("should see vote that was picked, in Team: section, upon clickig the 'Show votes'", () => {
  //   cy.visit("https://example.cypress.io");
  // });
  // it("should clear vote upon clickin 'Clear votes'", () => {
  //   cy.visit("https://example.cypress.io");
  // });
  // it("should skip story upon clickin 'Skip story'", () => {
  //   cy.visit("https://example.cypress.io");
  // });
  // it("should reset timer to 00:00 upon clickin on 'Reset timer'", () => {
  //   cy.visit("https://example.cypress.io");
  // });
  // it("should be taken to Chart step upon clicking on 'Finish Voting'", () => {
  //   cy.visit("https://example.cypress.io");
  // });
  // it("should render caution notification when you are on last story", () => {
  //   cy.visit("https://example.cypress.io");
  // });
  // it("should toggle from Bar chart to Donut chart", () => {
  //   cy.visit("https://example.cypress.io");
  // });
});
