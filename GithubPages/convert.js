// array of objects -> string
// converts the object array to bootstrap cards
function convertToCards(array) {
    let leadingHTML = '<div class="row justify-content-center">\n';
    let bodyHTML = "";
    const endHTML = `</div>\n`;
    array.forEach(obj => bodyHTML+= createCard(obj));

    return leadingHTML + bodyHTML + endHTML;
}


// obj -> string
// coverts an obj to a bootstrap card
function createCard(obj) {
    const leadingHTML = '<div class="col-lg-4 col-md-6 pb-3">\n<div class="card" style="width: 18rem;">\n<div class="card-body">\n';
    const endHTML = `</div>\n</div>\n</div>\n`;
    let bodyHTML = "";
    for (key in obj) {
        switch (key) {
            case 'title':
                bodyHTML += `<h5 class="card-title">${obj[key]}</h5>\n`;
                break;
            case 'authors':
                bodyHTML += `<h6 class="card-subtitle mb-2 text-muted">${obj[key]}</h6>\n`;
                break;
            case 'description':
                bodyHTML += `<p class="card-text">${obj[key]}</p>\n`;
                break;
            case 'link':
                bodyHTML += `<a href="${obj[key]}" class="card-link">Link to article</a>\n`;
                break;
            default:
                return; // unreachable
        }
    }
    return leadingHTML + bodyHTML + endHTML;
}



// array of objects -> string
// converts the object array to a bootstrap table
function ConvertToTable(array) {
    const leadingHTML = `<ul class="list-group">\n`;
    const endHTML = `</ul>\n</div>\n`;
    let bodyHTML = "";
    array.forEach(obj => bodyHTML += createTableRow(obj));

    return leadingHTML + bodyHTML + endHTML;
}

// obj -> string
// coverts an obj to a bootstrap table row
// NOTE: We only take the 5 most recent
function createTableRow(obj) {
    const leadingHTML = `<li class="list-group-item">Version${obj.version}\n<ul>\n`;
    const endHTML = `</ul>\n</li>\n`;
    let bodyHTML = "";
    console.log(obj);
    let index = 0;
    while (index < 5 && index < obj.notes.length) {
        bodyHTML += `<li>${obj.notes[index]}</li>\n`   
        index++;
    }
    return leadingHTML + bodyHTML + endHTML;
}


// html, document-query-selector -> html
// renders the html string as html 
function render(template, node) {
    node.innerHTML = template;
}


// converts the Publications object to a html card layout
function renderCardData(data) {
    const html = convertToCards(data);
    render(html, document.getElementById("publications"));
}

// converts the version history to a html table
function renderTableData(data) {
    const html = ConvertToTable(data);
    render(html, document.getElementById("history"));
}
