
--Cleaning Data in SQL
--change datetime to date

Select *
From PortfolioProject.dbo.NashvilleHousing;


ALTER TABLE NashvilleHousing
Add SaleDateConverted Date;

Update NashvilleHousing
SET SaleDateConverted = CONVERT(Date,SaleDate)


Select saleDateConverted, CONVERT(Date,SaleDate)
From PortfolioProject.dbo.NashvilleHousing

---------------------------------------------------------------------------------------------------------------------------------------------------

-- Populate Property Address data
select PropertyAddress
 from PortfolioProject.dbo.NashvilleHousing
 where PropertyAddress is null


select * 
 from PortfolioProject.dbo.NashvilleHousing
 order by ParcelID

 /*
 Similar percel ID have similar addreses so every null address that has similar parcel ID with those that
 are populated will be populated with the the same address
 */

 select  a.ParcelID, a.PropertyAddress, b.ParcelID, b.PropertyAddress, ISNULL(a.PropertyAddress, b.PropertyAddress)
 from PortfolioProject.dbo.NashvilleHousing a
 join PortfolioProject.dbo.NashvilleHousing b
 on a.ParcelID = b.ParcelID
 and a.[UniqueID] <> b.[UniqueID]
 where a.PropertyAddress is null


 update a
 set PropertyAddress = ISNULL(a.PropertyAddress, b.PropertyAddress)
 from PortfolioProject.dbo.NashvilleHousing a
 join PortfolioProject.dbo.NashvilleHousing b
 on a.ParcelID = b.ParcelID
 and a.[UniqueID] <> b.[UniqueID]
 where a.PropertyAddress is null


 -----------------------------------------------------------------------------------------------------------------------------------------------------------


 --Breaking Address into individual columns (Address, City, State)

 select PropertyAddress
 from PortfolioProject.dbo.NashvilleHousing
--order by ParcelID

select
SUBSTRING(PropertyAddress, 1, CHARINDEX(',', PropertyAddress) -1 ) as Address
, SUBSTRING(PropertyAddress, CHARINDEX(',', PropertyAddress) +1 , LEN(PropertyAddress)) as Address
from PortfolioProject.dbo.NashvilleHousing


ALTER TABLE NashvilleHousing
Add PropertySplitAddress Nvarchar(255);

--ALTER TABLE NashvilleHousing
--drop column PropertySplitAddress;

Update NashvilleHousing
SET PropertySplitAddress = SUBSTRING(PropertyAddress, 1, CHARINDEX(',', PropertyAddress) -1) 


ALTER TABLE NashvilleHousing
Add PropertySplitCity Nvarchar(255);

--ALTER TABLE NashvilleHousing
--drop column PropertySplitCity;

Update NashvilleHousing
SET  PropertySplitCity = SUBSTRING(PropertyAddress, CHARINDEX(',', PropertyAddress) +1 , LEN(PropertyAddress)) 


 select *
 from PortfolioProject.dbo.NashvilleHousing



 select OwnerAddress
 from PortfolioProject.dbo.NashvilleHousing


 select
 PARSENAME(REPLACE(OwnerAddress,',','.'),3)
 ,PARSENAME(REPLACE(OwnerAddress,',','.'),2)
 ,PARSENAME(REPLACE(OwnerAddress,',','.'),1)
  from PortfolioProject.dbo.NashvilleHousing


  ALTER TABLE NashvilleHousing
Add OwnerSplitAddress Nvarchar(255);

Update NashvilleHousing
SET OwnerSplitAddress = PARSENAME(REPLACE(OwnerAddress,',','.'),3)

ALTER TABLE NashvilleHousing
Add OwnerSplitCity Nvarchar(255);

Update NashvilleHousing
SET  OwnerSplitCity = PARSENAME(REPLACE(OwnerAddress,',','.'),2)

ALTER TABLE NashvilleHousing
Add OwnerSplitState Nvarchar(255);

Update NashvilleHousing
SET OwnerSplitState = PARSENAME(REPLACE(OwnerAddress,',','.'),1) 


 select *
 from PortfolioProject.dbo.NashvilleHousing


 ----------------------------------------------------------------------------------------------------------------------------------------------------------

 --Change Y and N to Yes and No in "Sold as vacant" field


 select Distinct(SoldAsVacant), count(SoldAsVacant)
  from PortfolioProject.dbo.NashvilleHousing
  group by SoldAsVacant
  order by 2

select SoldAsVacant
 , Case when SoldAsVacant = 'Y' then 'Yes'
        when SoldAsVacant = 'N' then 'No'
		else SoldAsVacant
		end
   from PortfolioProject.dbo.NashvilleHousing

Update NashvilleHousing
set SoldAsVacant =  Case when SoldAsVacant = 'Y' then 'Yes'
        when SoldAsVacant = 'N' then 'No'
		else SoldAsVacant
		end
   --from PortfolioProject.dbo.NashvilleHousing


------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

--Remove duplicates


WITh RowNumCTE AS(
select *,
    ROW_NUMBER() OVER(
	PARTITION BY ParcelID,
	             PropertyAddress,
				 SalePrice,
				 SaleDate,
				 LegalReference
				 ORDER BY
				    UniqueID
					)row_num
from PortfolioProject.dbo.NashvilleHousing
--order by ParcelID
)
select*
from RowNumCTE
where row_num > 1
order by PropertyAddress


WITh RowNumCTE AS(
select *,
    ROW_NUMBER() OVER(
	PARTITION BY ParcelID,
	             PropertyAddress,
				 SalePrice,
				 SaleDate,
				 LegalReference
				 ORDER BY
				    UniqueID
					)row_num
from PortfolioProject.dbo.NashvilleHousing
--order by ParcelID
)
Delete
from RowNumCTE
where row_num > 1
--order by PropertyAddress


-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

--Delete Unused columns



select *
 from PortfolioProject.dbo.NashvilleHousing


ALTER TABLE NashvilleHousing
drop column OwnerAddress, TaxDistrict, PropertyAddress

ALTER TABLE NashvilleHousing
drop column SaleDate