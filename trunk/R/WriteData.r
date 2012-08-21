$header = array(
	'object'=>array(
		'name' => "Ahaaa",
		'ident' => "testi.kaavii",
		'subset_name' => 'Kaavi',
		'type' => "variable",
		'page' => 123578,
		'wiki_id' =>1
	),
	'act'=>array(
		'unit' => "kg/cm3",
		'who' => "Alberto",
		'samples' => 1,
		'comments' => "Tämä on kommentti tähän actiin",
		'language' => 'eng'
	),
	'indices'=>array(
		1=>array('type'=>'entity','name'=>'Indeksi ykkönen','page'=>666,'wiki_id'=>1,'order_index'=>1,'hidden'=>0,’unit’=>’brand’),
		2=>array('type'=>'number','name'=>'Indeksi kakkonen','page'=>777,'wiki_id'=>2,'order_index'=>2,'hidden'=>0,’unit’=>’cm’),
		3=>array('type'=>'time','name'=>'Indeksi kolmonen','page'=>888,'wiki_id'=>1,'order_index'=>3,'hidden'=>1),
	)                
);

 

 

 

$data['indices']=array(
	1=>array('name'=>'Indeksi ykkönen'),
	2=>array('name'=>'Indeksi kakkonen'),
	3=>array('name'=>'Indeksi kolmonen')
);
 

for ($i = 1; $i < 500; $i++)
	$data['data'][$i] = array(1 => 'porsche',2 => 123456.67,3 => "2011-05-13T14:22:46.12+02:00", 'res' => 123, 'mean' => 123, 'sd' => 123);



	$ret = json_decode(do_post_request('http://cl1.opasnet.org/opasnet_base_2/index.php', http_build_query(array('json' => json_encode($header)))));
	if ($ret && ! isset($ret->error) && ! empty($ret->key)) {
		echo "Token: " . $ret->key;
		echo "<br/><br/>POSTing data<br/>";
		$data['key'] = $ret->key;
		$ret = do_post_request('http://cl1.opasnet.org/opasnet_base_2/index.php', http_build_query(array('json' => json_encode($data))));
	}



##############
# WriteData
###############
# op_baseWrite equivalent for opasnet base 2
################

WriteData <- function(data, ident) {
	
	
	
	header <- list(
		object = list(
			asd
		),
		act = list(
			asd
		),
		indices
	)
	postToHost
}

























