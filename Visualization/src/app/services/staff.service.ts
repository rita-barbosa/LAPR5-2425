import { Injectable } from '@angular/core';
import { MessageService } from './message.service';
import { HttpClient, HttpHeaders } from '@angular/common/http';
import { Staff } from '../domain/staff';
import { catchError, map, Observable, of } from 'rxjs';
import { Specialization } from '../domain/specialization';

@Injectable({
  providedIn: 'root'
})
export class StaffService {
  theServerURL = 'https://localhost:5001/api';
  httpOptions = {
    headers: new HttpHeaders({'Content-Type': 'application/json' })
  };
  
  constructor(private messageService: MessageService, private http: HttpClient) { }

  public getAllSpecializationsAvailable(): Observable<string[]> {
    const url = `${this.theServerURL}/Specializations`;
  
    return this.http.get<Specialization[]>(url, this.httpOptions)
                    .pipe(
                      map(data => data.map(spec => spec.denomination)),
                      catchError(this.handleError<string[]>('Get Specializations', []))
                    );
  }
  public createStaffProfile(firstName: string, lastName: string, phone: string, email: string, address: string, licenseNumber: string, specialization: string,functionDescrip:string) {
    const url = `${this.theServerURL}/Staff/Create-StaffProfile`;
    let staff: Staff = {
      licenseNumber: licenseNumber,
      firstName: firstName,
      lastName: lastName,
      phone: phone,
      email: email,
      address: address,
      function: functionDescrip,
      specializationId: specialization
    };

    this.http.post<Staff>(url, staff, this.httpOptions)
      .pipe(catchError(this.handleError<Staff>('Create staff profile')))
      .subscribe(data => {
        this.log(`Staff profile: ${data.email} was successfully created.`);
      });
  }

  //------------------------/------------------------/------------------------
  private handleError<T>(operation = 'operation', result?: T) {
    return (error: any): Observable<T> => {

      if (error.status === 440) {
        this.log("Error: Login session expired.");
        return of(result as T);
      } else if (error.status === 401) {
        this.log("Error: Authentication is required.");
        return of(result as T);
      } else if (error.status === 403) {
        this.log("Error: Not allowed to access the feature.");
        return of(result as T);
      } else if(error.status === 200 ||error.status === 400 ){
        this.log(`${operation} failed: ${error.error}`);
      } else {
        this.log(`${operation} failed: an unexpected error occured.`);
      }
      return of(result as T);
    };
  }
  private log(message: string) {
    this.messageService.add(`${message}`);
  }
}
