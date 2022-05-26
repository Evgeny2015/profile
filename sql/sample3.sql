create procedure request.addarticle
	@list_id int,
	@store_id int,
	@cnt int,
	@amount numeric (19, 9)
as
begin	
	set nocount on
	
	declare 		
		@art_id int,
		@article int,
		@item_id int,
		@res_id int,
		@rest numeric (19, 9),
		@units varchar(8)
		
	select @article = art_id, @rest = amount - store.getreserve(@store_id, 0) from as_store where store_id = @store_id
	if @amount = 0 set @amount = @rest
	if @amount <= 0 or @amount > @rest
		throw 60000, 'request.addarticle Недопустимое значение атрибута', @amount;
		
	select @item_id = item_id, @units = units from as_article where art_id = @article			
	
		
	begin tran
	begin try

	-- add article	
	insert into as_article (list_id, item_id, units) values (@list_id, @item_id, @units)	
	set @art_id = @@identity
	
	-- add request property
	insert into as_riprop (art_id, article, amount) values (@art_id, @article, @amount)
		
	-- add request reserve	
	exec store.addreserve @store_id, @cnt, @amount, 0x04, @res_id output	
	insert into request.reserve (res_id, art_id) values (@res_id, @art_id)	
	
	end try
	begin catch
		if @@trancount > 0
			rollback tran

		declare @ErrorMessage nvarchar(4000)
		declare @ErrorSeverity int
		declare @ErrorState int

		select @ErrorMessage = error_message(), @ErrorSeverity = error_severity(), @ErrorState = error_state();

		raiserror (@ErrorMessage, @ErrorSeverity, @ErrorState);
	end catch
	
	if @@trancount > 0
		commit tran	
        
	return @art_id
end
go