alter procedure deficit.Recover
	@stage_id int,
	@cnt int,
	@store_id int
as
begin
	set nocount on

	declare
		@rate numeric (19, 9) = (
			select a.amt*a.rate 
			from as_works a
			inner join reserve.stage b on a.work_id = b.work_id
			where b.stage_id = @stage_id
			),
		@deficitCount int = (
			select deficit from reserve.stage where stage_id = @stage_id
			),	
		@restAmount numeric (19, 9) = (		
			select amount - store.getreserve(@store_id, 0x00) from as_store where store_id = @store_id
			)
	
	declare
		@reserveAmount numeric (19, 9) = @cnt*@rate,
		@res_id int
		
	begin tran
	begin try		
	
	if @cnt <= 0 or @cnt > @deficitCount return -1
	if @reserveAmount > @restAmount return -2
	
	-- add recovery reserve	
	exec store.addreserve @store_id, @cnt, @reserveAmount, 0x08, @res_id output	
	insert into reserve.reserve (res_id, stage_id) values (@res_id, @stage_id)	
	
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
		
	return 0
end
go