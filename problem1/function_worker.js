
onmessage = function(e) {
    // the passed-in data is available via e.data
    const {f, args} = e.data;
    let i = f.indexOf("(") + 1;
    let j = f.indexOf(")", i);
    let params = f.substring(i, j).split(",");

    i = f.indexOf("{") + 1 ;
    j = f.lastIndexOf("}");
    let body = f.substring(i, j);

    let g = new Function(params, body);
    z = g.apply(null,args);
    self.postMessage(z);
};



