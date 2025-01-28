venv = new Map([
	["hasBoughtHouse",false],
	["hasMaintLoan",false],
	["hasSoldHouse",false],
	["zip",],
	["sellingPrice",0],
	["privateDebt",0],
	["valueResidue",0],
	["ifcondition0",false],
]);

const mapsAreEqual = (m1, m2) => m1.size === m2.size && Array.from(m1.keys()).every((key) => m1.get(key) === m2.get(key));

window.onload = update();

function evaluateInput(value, id) {
  	venv.set(id,value);
  	update();
}

function update(){
  solve();
  setContent();
}

function calculate(){
	venv.set("valueResidue", (Number(venv.get("sellingPrice")) - Number(venv.get("privateDebt"))));
	venv.set("ifcondition0", venv.get("hasSoldHouse"));
}

function setContent(){
	document.getElementsByName("privateDebt")[0].max = venv.get("sellingPrice")
	document.getElementsByName("privateDebt")[0].min = 0
	document.getElementsByName("valueResidue")[0].textContent = venv.get("valueResidue");
	document.getElementsByName("ifcondition0")[0].style.visibility = venv.get("hasSoldHouse") ? "visible" : "hidden";
	Array.prototype.forEach.call(document.getElementsByName("ifcondition0")[0].getElementsByTagName("input"),target => target.disabled = !venv.get("hasSoldHouse"));
}

function solve(){
  do {
    temp = venv;
    calculate();
  } while (!mapsAreEqual(temp,venv));
}