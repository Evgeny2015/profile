ALTER procedure [omts].[GetDeficit]
-- Get  deficit for all completed plans
as
begin
	set nocount on	
		
	declare @article table (art_id int, list_id int, item_id int, fromset int, amount int, index list_idx (list_id), primary key (art_id))
	declare @list table (list_id int, primary key (list_id))
	declare @works table (work_id int, art_id int, item_id int, cnt int, rate numeric (19, 9), [state] smallint, index art_key (art_id), primary key (work_id))
	declare @items table (item_id int, deficit numeric (19, 9), primary key(item_id))
	
	-- get plan list
	insert into @list (list_id)
	select a.list_id
	from as_lnew a	
	inner join as_list b on a.list_id = b.list_id	
	where a.doctype_id = 104 
	and b.completed is not null

	-- get articles 
	insert into @article (art_id, list_id, item_id, fromset, amount)
	select b.art_id, a.list_id, b.item_id, c.produced + 1, c.amount - c.produced
	from @list a
	inner join as_article b on a.list_id = b.list_id
	cross apply omts.getArtAmount(b.art_id) c

	-- get works	
	insert into @works (work_id, art_id, item_id, cnt, rate, [state])
	select b.work_id, a.art_id, c.child, b.cnt, c.amt*c.rate, c.[state]
	from @article a
	cross apply dbo.tf_getworks (a.art_id, a.item_id, a.fromset, a.amount) b
	inner join as_works c on b.work_id = c.work_id

	insert into @items (item_id, deficit)
	select a.item_id, sum(a.deficit)
	from (
		-- get plan deficit
		select a.item_id, omts.getItemDeficit(a.item_id, a.rate) deficit
		from (
			select a.item_id, sum(a.cnt*a.rate) rate
			from @works a 		
			group by a.item_id	
		) a
		union all
		-- get production deficit
		select a.item_id, sum(a.rate*(b1.deficit + b2.deficit + b3.deficit))
		from @works a
		inner join reserve.stage b1 on a.work_id = b1.work_id and b1.stage = 0x00
		inner join reserve.stage b2 on a.work_id = b2.work_id and b2.stage = 0x10
		inner join reserve.stage b3 on a.work_id = b2.work_id and b2.stage = 0x20
		group by a.item_id
	) a
	where a.deficit > 0
	group by a.item_id

	select (
	select 
		-- get plans
		(select a.list_id '@id', b.num '@num', convert(char(6), b.[date], 12) '@date'
		from @list a
		inner join as_list b on a.list_id = b.list_id
		for xml path('item'), type
		) plans,

		-- get articles
		(select a.art_id '@id', a.list_id '@lid', c.[name] '@name'
		from @article a
		inner join as_article b on a.art_id = b.art_id
		inner join as_items c on b.item_id = c.item_id
		for xml path('item'), type
		) arts,

		-- get works
		( select a.art_id '@aid', a.work_id '@id', a.item_id '@iid', a.cnt '@cnt', a.rate '@rate'
		from @works a
		inner join @items b on a.item_id = b.item_id
		for xml path('item'), type		
		) as works,

		-- get deficit
		(select a.item_id '@id', a.deficit '@un'
		from @items a				
		for xml path('item'), type
		) as def, 

		-- get items
		(select a.item_id '@id', a.nota '@nota', a.[name] '@name', dbo.tp_getrepunit(a.units) '@un', a.price '@pr'			
			, c.cs '@cs'
			, d.[is] '@is'
			, e.ss '@ss'
		from as_items a
		inner join @items b on a.item_id = b.item_id
		cross apply (
			select sum(b.amount) cs
			from as_store b 
			inner join as_article c on b.art_id = c.art_id 
			where c.item_id = a.item_id and b.source_id = 1001
		) c
		cross apply (
			select sum(b.amount) [is]
			from as_store b 
			inner join as_article c on b.art_id = c.art_id 
			where c.item_id = a.item_id and b.source_id = 1002
		) d
		cross apply (
			select sum(b.amount) ss
			from as_store b 
			inner join as_article c on b.art_id = c.art_id 
			where c.item_id = a.item_id and b.source_id = 1003
		) e
		for xml path('item'), type
		) items


	for xml path('report')
	) as xml

	return 0
end
go